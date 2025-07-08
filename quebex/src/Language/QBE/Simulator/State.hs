-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.State where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, modify)
import Data.Array.IO (IOArray)
import Data.Map qualified as Map
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.Tracer qualified as T
import Language.QBE.Types qualified as QBE

type Exec val tracer ret = StateT (Env val tracer) (ExceptT EvalError IO) ret

-- TODO: Move this elsewhere or just provide a Maybe -> Exec lifter

toAddressE :: (E.ValueRepr v) => v -> Exec v t MEM.Address
toAddressE addr =
  case E.toAddress addr of
    Nothing -> throwError InvalidAddressType
    Just rt -> pure rt

subTypeE :: (E.ValueRepr v) => QBE.BaseType -> v -> Exec v t v
subTypeE ty v =
  case E.subType ty v of
    Nothing -> throwError TypingError
    Just rt -> pure rt

swToLongE :: (E.ValueRepr v) => QBE.SubWordType -> v -> Exec v t v
swToLongE ty v =
  case E.swToLong ty v of
    Nothing -> throwError TypingError
    Just rt -> pure rt

wordToLongE :: (E.ValueRepr v) => QBE.SubLongType -> v -> Exec v t v
wordToLongE ty v =
  case E.wordToLong ty v of
    Nothing -> throwError InvaldSubWordExtension
    Just rt -> pure rt

runBinary :: (E.ValueRepr v) => QBE.BaseType -> (v -> v -> Maybe v) -> v -> v -> Exec v t v
runBinary ty op lhs rhs = do
  lhs' <- subTypeE ty lhs
  rhs' <- subTypeE ty rhs
  case op lhs' rhs' of
    Nothing -> throwError TypingError
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

data Env val tracer
  = Env
  { envGlobals :: Map.Map QBE.GlobalIdent val,
    envFuncs :: Map.Map QBE.GlobalIdent QBE.FuncDef,
    envMem :: MEM.Memory IOArray val,
    envStk :: [StackFrame val],
    envStkPtr :: MEM.Address,
    envTracer :: tracer
  }

mkEnv :: (T.Tracer t v) => [QBE.FuncDef] -> MEM.Address -> MEM.Size -> t -> IO (Env v t)
mkEnv funcs a s t = do
  mem <- MEM.mkMemory a s
  return $ Env Map.empty (makeFuncs funcs) mem [] (s - 1) t
  where
    makeFuncs :: [QBE.FuncDef] -> Map.Map QBE.GlobalIdent QBE.FuncDef
    makeFuncs = Map.fromList . map (\f -> (QBE.fName f, f))

activeFrame :: Exec v t (StackFrame v)
activeFrame = do
  env <- get
  case env of
    Env {envStk = x : _} -> pure x
    _ -> throwError EmptyStack

modifyFrame :: (StackFrame v -> StackFrame v) -> Exec v t ()
modifyFrame func = do
  stack <- gets envStk
  case stack of
    (x : xs) -> modify (\s -> s {envStk = func x : xs})
    _ -> throwError EmptyStack

pushStackFrame :: QBE.FuncDef -> Exec v t ()
pushStackFrame f = do
  stkPtr <- gets envStkPtr
  modify (\s -> s {envStk = mkStackFrame f stkPtr : envStk s})

popStackFrame :: Exec v t ()
popStackFrame = do
  stk <- gets envStk
  case stk of
    [] -> pure ()
    ((StackFrame {stkFp = fp}) : xs) ->
      modify (\s -> s {envStk = xs, envStkPtr = fp})

stackAlloc :: (E.ValueRepr v) => MEM.Size -> MEM.Address -> Exec v t v
stackAlloc size align = do
  stkPtr <- gets envStkPtr
  let newStkPtr = alignAddr (stkPtr - size) align
  modify (\s -> s {envStkPtr = newStkPtr})
  return $ E.fromAddress newStkPtr
  where
    alignAddr :: MEM.Address -> MEM.Address -> MEM.Address
    alignAddr addr alignment = addr - (addr `mod` alignment)

writeMemory :: (E.Storable v) => MEM.Address -> QBE.ExtType -> v -> Exec v t ()
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

readMemory :: (E.Storable v) => QBE.LoadType -> MEM.Address -> Exec v t v
readMemory ty addr = do
  mem <- gets envMem
  bytes <- liftIO $ MEM.loadBytes mem addr (QBE.loadByteSize ty)

  case E.fromBytes (QBE.loadToExtType ty) bytes of
    Just x -> pure x
    Nothing -> throwError InvalidMemoryLoad

maybeLookup :: String -> Maybe v -> Exec v t v
maybeLookup _ (Just x) = pure x
maybeLookup name Nothing = throwError $ UnknownVariable name

lookupValue :: (E.ValueRepr v) => QBE.BaseType -> QBE.Value -> Exec v t v
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

lookupFunc :: QBE.Value -> Exec v t QBE.FuncDef
lookupFunc (QBE.VConst (QBE.Const (QBE.Global name))) = do
  funcs <- gets envFuncs
  case Map.lookup name funcs of
    Just def -> pure def
    Nothing -> throwError (UnknownFunction name)
lookupFunc _ = error "non-global functions not supported"

lookupParam :: (E.ValueRepr v) => QBE.FuncParam -> Exec v t v
lookupParam (QBE.Regular abity ident) = do
  lookupValue (QBE.abityToBase abity) (QBE.VLocal ident)
lookupParam (QBE.Env _) = error "env function parameters not supported"
lookupParam QBE.Variadic = error "variadic functions not supported"

lookupParams :: (E.ValueRepr v) => [QBE.FuncParam] -> Exec v t [v]
lookupParams = mapM lookupParam

------------------------------------------------------------------------

-- TODO: Refactor with generic lift function

trackBranch :: (T.Tracer t v, E.ValueRepr v) => v -> Bool -> Exec v t ()
trackBranch condValue condResult = do
  let newTracer t = T.branch t condValue condResult
  modify (\s@Env {envTracer = t} -> s {envTracer = newTracer t})
