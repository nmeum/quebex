-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.State where

import Data.Functor ((<&>))
import Data.List (find)
import Control.Exception (throwIO)
import Control.Monad.Cont (ContT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, modify)
import Data.Array.IO (IOArray)
import Data.Map qualified as Map
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.Tracer qualified as T
import Language.QBE.Types qualified as QBE

-- control flow hole
data CFHole v = ReturnValue (Maybe v)
            | JumpTo QBE.BlockIdent
            | TakeBranch v QBE.BlockIdent QBE.BlockIdent
            | Halt

type Exec val byteTy tracer ret = ContT () (StateT (Env val tracer byteTy) IO) ret

type State v = StateT (Env' v) IO

type Exec' v r a = ContT r (State v) a

-- TODO: Move this elsewhere or just provide a Maybe -> Exec lifter

toAddressE :: (E.ValueRepr v) => v -> Exec v b t MEM.Address
toAddressE addr =
  case E.toAddress addr of
    Nothing -> liftIO $ throwIO InvalidAddressType
    Just rt -> pure rt

subTypeE :: (E.ValueRepr v) => QBE.BaseType -> v -> Exec v b t v
subTypeE ty v =
  case E.subType ty v of
    Nothing -> liftIO $ throwIO TypingError
    Just rt -> pure rt

subTypeE' :: (E.ValueRepr v) => QBE.BaseType -> v -> State v v
subTypeE' ty v =
  case E.subType ty v of
    Nothing -> liftIO $ throwIO TypingError
    Just rt -> pure rt

wordToLongE :: (E.ValueRepr v) => QBE.SubLongType -> v -> Exec v b t v
wordToLongE ty v =
  case E.wordToLong ty v of
    Nothing -> liftIO $ throwIO InvaldSubWordExtension
    Just rt -> pure rt

runBinary :: (E.ValueRepr v) => QBE.BaseType -> (v -> v -> Maybe v) -> v -> v -> Exec v b t v
runBinary ty op lhs rhs = do
  lhs' <- subTypeE ty lhs
  rhs' <- subTypeE ty rhs
  case op lhs' rhs' of
    Nothing -> liftIO $ throwIO TypingError
    Just rt -> pure rt

------------------------------------------------------------------------

type RegMap v = Map.Map QBE.LocalIdent v

data StackFrame v
  = StackFrame
  { stkFunc :: QBE.FuncDef,
    stkVars :: RegMap v,
    stkFp :: MEM.Address
  }

mkStackFrame :: QBE.FuncDef -> MEM.Address -> StackFrame v
mkStackFrame func = StackFrame func Map.empty

storeLocal :: QBE.LocalIdent -> v -> StackFrame v -> StackFrame v
storeLocal ident value frame@(StackFrame {stkVars = v}) =
  frame {stkVars = Map.insert ident value v}

lookupLocal :: StackFrame v -> QBE.LocalIdent -> Maybe v
lookupLocal (StackFrame {stkVars = v}) = flip Map.lookup v

------------------------------------------------------------------------

data Env val tracer byteTy
  = Env
  { envGlobals :: Map.Map QBE.GlobalIdent val,
    envFuncs :: Map.Map QBE.GlobalIdent QBE.FuncDef,
    envMem :: MEM.Memory IOArray byteTy,
    envStk :: [StackFrame val],
    envStkPtr :: MEM.Address,
    envTracer :: tracer
  }

mkEnv :: (E.Storable v b, T.Tracer t v) => [QBE.FuncDef] -> MEM.Address -> MEM.Size -> t -> IO (Env v t b)
mkEnv funcs a s t = do
  mem <- MEM.mkMemory a s
  return $ Env Map.empty (makeFuncs funcs) mem [] (s - 1) t
  where
    makeFuncs :: [QBE.FuncDef] -> Map.Map QBE.GlobalIdent QBE.FuncDef
    makeFuncs = Map.fromList . map (\f -> (QBE.fName f, f))

data Env' v
  = Env'
  { envGlobals' :: Map.Map QBE.GlobalIdent v,
    envFuncs' :: Map.Map QBE.GlobalIdent QBE.FuncDef,
    envStk' :: [StackFrame v],
    envStkPtr' :: MEM.Address
  }

mkEnv' :: (E.ValueRepr v) => [QBE.FuncDef] -> IO (Env' v)
mkEnv' funcs = do
  return $ Env' Map.empty (makeFuncs funcs) [] 0
  where
    makeFuncs :: [QBE.FuncDef] -> Map.Map QBE.GlobalIdent QBE.FuncDef
    makeFuncs = Map.fromList . map (\f -> (QBE.fName f, f))

activeFrame' :: State v (StackFrame v)
activeFrame' = do
  env <- get
  case env of
    Env' {envStk' = x : _} -> pure x
    _ -> liftIO $ throwIO EmptyStack

activeFrame :: Exec v b t (StackFrame v)
activeFrame = do
  env <- get
  case env of
    Env {envStk = x : _} -> pure x
    _ -> liftIO $ throwIO EmptyStack

modifyFrame :: (StackFrame v -> StackFrame v) -> Exec v b t ()
modifyFrame func = do
  stack <- gets envStk
  case stack of
    (x : xs) -> modify (\s -> s {envStk = func x : xs})
    _ -> liftIO $ throwIO EmptyStack

pushStackFrame :: QBE.FuncDef -> Exec v b t ()
pushStackFrame f = do
  stkPtr <- gets envStkPtr
  modify (\s -> s {envStk = mkStackFrame f stkPtr : envStk s})

popStackFrame :: Exec v b t ()
popStackFrame = do
  stk <- gets envStk
  case stk of
    [] -> pure ()
    ((StackFrame {stkFp = fp}) : xs) ->
      modify (\s -> s {envStk = xs, envStkPtr = fp})

stackAlloc :: (E.ValueRepr v) => MEM.Size -> MEM.Address -> Exec v b t v
stackAlloc size align = do
  stkPtr <- gets envStkPtr
  let newStkPtr = alignAddr (stkPtr - size) align
  modify (\s -> s {envStkPtr = newStkPtr})
  return $ E.fromAddress newStkPtr
  where
    alignAddr :: MEM.Address -> MEM.Address -> MEM.Address
    alignAddr addr alignment = addr - (addr `mod` alignment)

writeMemory :: (E.Storable v b) => MEM.Address -> QBE.ExtType -> v -> Exec v b t ()
writeMemory addr extType regVal = do
  mem <- gets envMem

  -- Since halfwords and bytes are not first class in the IL, storeh and storeb
  -- take a word as argument. Only the first 16 or 8 bits of this word will be
  -- stored in memory at the address specified in the second argument.
  let bytes = E.toBytes regVal
  liftIO $
    MEM.storeBytes mem addr $
      case extType of
        QBE.Byte -> take 1 bytes
        QBE.HalfWord -> take 2 bytes
        QBE.Base _ -> bytes

readMemory :: (E.Storable v b) => QBE.LoadType -> MEM.Address -> Exec v b t v
readMemory ty addr = do
  mem <- gets envMem
  bytes <- liftIO $ MEM.loadBytes mem addr (QBE.loadByteSize ty)

  case E.fromBytes ty bytes of
    Just x -> pure x
    Nothing -> liftIO $ throwIO InvalidMemoryLoad

findBlock :: (E.ValueRepr v) => QBE.BlockIdent -> State v QBE.Block
findBlock ident = do
  blocks <- QBE.fBlock <$> (activeFrame' <&> stkFunc)
  case find (\x -> QBE.label x == ident) blocks of
    Just bl -> pure bl
    Nothing -> liftIO $ throwIO (UnknownBlock ident)

maybeLookup' :: String -> Maybe v -> State v v
maybeLookup' _ (Just x) = pure x
maybeLookup' name Nothing = liftIO $ throwIO $ UnknownVariable name

maybeLookup :: String -> Maybe v -> Exec v b t v
maybeLookup _ (Just x) = pure x
maybeLookup name Nothing = liftIO $ throwIO $ UnknownVariable name

lookupValue' :: (E.ValueRepr v) => QBE.BaseType -> QBE.Value -> State v v
lookupValue' ty (QBE.VConst (QBE.Const (QBE.Number v))) =
  pure $ E.fromLit ty v
lookupValue' ty (QBE.VConst (QBE.Const (QBE.SFP v))) =
  subTypeE' ty (E.fromFloat v)
lookupValue' ty (QBE.VConst (QBE.Const (QBE.DFP v))) =
  subTypeE' ty (E.fromDouble v)
lookupValue' ty (QBE.VConst (QBE.Const (QBE.Global k))) = do
  v <- gets envGlobals' >>= maybeLookup' (show k) . Map.lookup k
  subTypeE' ty v
lookupValue' ty (QBE.VConst (QBE.Thread k)) = do
  v <- gets envGlobals' >>= maybeLookup' (show k) . Map.lookup k
  subTypeE' ty v
lookupValue' ty (QBE.VLocal k) = do
  v <- activeFrame' >>= maybeLookup' (show k) . flip lookupLocal k
  subTypeE' ty v

lookupValue :: (E.ValueRepr v) => QBE.BaseType -> QBE.Value -> Exec v b t v
lookupValue ty (QBE.VConst (QBE.Const (QBE.Number v))) =
  pure $ E.fromLit ty v
lookupValue ty (QBE.VConst (QBE.Const (QBE.SFP v))) =
  subTypeE ty (E.fromFloat v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.DFP v))) =
  subTypeE ty (E.fromDouble v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.Global k))) = do
  v <- gets envGlobals >>= maybeLookup (show k) . Map.lookup k
  subTypeE ty v
lookupValue ty (QBE.VConst (QBE.Thread k)) = do
  v <- gets envGlobals >>= maybeLookup (show k) . Map.lookup k
  subTypeE ty v
lookupValue ty (QBE.VLocal k) = do
  v <- activeFrame >>= maybeLookup (show k) . flip lookupLocal k
  subTypeE ty v

lookupFunc :: QBE.Value -> Exec v b t QBE.FuncDef
lookupFunc (QBE.VConst (QBE.Const (QBE.Global name))) = do
  funcs <- gets envFuncs
  case Map.lookup name funcs of
    Just def -> pure def
    Nothing -> liftIO $ throwIO (UnknownFunction name)
lookupFunc _ = error "non-global functions not supported"

lookupArg :: (E.ValueRepr v) => QBE.FuncArg -> Exec v b t v
lookupArg (QBE.ArgReg abity value) = do
  lookupValue (QBE.abityToBase abity) value
lookupArg (QBE.ArgEnv _) = error "env function parameters not supported"
lookupArg QBE.ArgVar = error "variadic functions not supported"

lookupArgs :: (E.ValueRepr v) => [QBE.FuncArg] -> Exec v b t [v]
lookupArgs = mapM lookupArg

------------------------------------------------------------------------

-- TODO: Refactor with generic lift function

trackBranch :: (T.Tracer t v, E.ValueRepr v) => v -> Bool -> Exec v b t ()
trackBranch condValue condResult = do
  let newTracer t = T.branch t condValue condResult
  modify (\s@Env {envTracer = t} -> s {envTracer = newTracer t})
