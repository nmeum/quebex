module Language.QBE.Simulator.State where

import Control.Monad.Except (ExceptT, liftEither, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, modify)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE

type Exec a = StateT Env (ExceptT EvalError IO) a

------------------------------------------------------------------------

type RegMap = Map.Map QBE.LocalIdent E.RegVal

data StackFrame
  = StackFrame
  { stkFunc :: QBE.FuncDef,
    stkVars :: RegMap,
    stkFp :: MEM.Address
  }

mkStackFrame :: QBE.FuncDef -> MEM.Address -> StackFrame
mkStackFrame func = StackFrame func Map.empty

storeLocal :: QBE.LocalIdent -> E.RegVal -> StackFrame -> StackFrame
storeLocal ident value frame@(StackFrame {stkVars = v}) =
  frame {stkVars = Map.insert ident value v}

lookupLocal :: StackFrame -> QBE.LocalIdent -> Maybe E.RegVal
lookupLocal (StackFrame {stkVars = v}) = flip Map.lookup v

------------------------------------------------------------------------

data Env
  = Env
  { envGlobals :: Map.Map QBE.GlobalIdent E.RegVal,
    envFuncs :: Map.Map QBE.GlobalIdent QBE.FuncDef,
    envMem :: MEM.Memory,
    envStk :: [StackFrame],
    envStkPtr :: MEM.Address
  }

mkEnv :: [QBE.FuncDef] -> MEM.Address -> MEM.Size -> IO Env
mkEnv funcs a s = do
  mem <- MEM.mkMemory a s
  return $ Env Map.empty (makeFuncs funcs) mem [] (s - 1)
  where
    makeFuncs :: [QBE.FuncDef] -> Map.Map QBE.GlobalIdent QBE.FuncDef
    makeFuncs = Map.fromList . map (\f -> (QBE.fName f, f))

activeFrame :: Exec StackFrame
activeFrame = do
  env <- get
  case env of
    Env {envStk = x : _} -> pure x
    _ -> throwError EmptyStack

modifyFrame :: (StackFrame -> StackFrame) -> Exec ()
modifyFrame func = do
  stack <- gets envStk
  case stack of
    (x : xs) -> modify (\s -> s {envStk = func x : xs})
    _ -> throwError EmptyStack

pushStackFrame :: QBE.FuncDef -> Exec ()
pushStackFrame f = do
  stkPtr <- gets envStkPtr
  modify (\s -> s {envStk = mkStackFrame f stkPtr : envStk s})

popStackFrame :: Exec ()
popStackFrame = do
  stk <- gets envStk
  case stk of
    [] -> pure ()
    ((StackFrame {stkFp = fp}) : xs) ->
      modify (\s -> s {envStk = xs, envStkPtr = fp})

stackAlloc :: MEM.Size -> MEM.Address -> Exec E.RegVal
stackAlloc size align = do
  stkPtr <- gets envStkPtr
  let newStkPtr = alignAddr (stkPtr - size) align
  modify (\s -> s {envStkPtr = newStkPtr})
  return $ E.VLong newStkPtr
  where
    alignAddr :: MEM.Address -> MEM.Address -> MEM.Address
    alignAddr addr alignment = addr - (addr `mod` alignment)

writeMemory :: MEM.Address -> E.RegVal -> Exec ()
writeMemory addr regVal = do
  mem <- gets envMem
  liftIO $ MEM.storeBytes mem addr (E.toBytes regVal)

readMemory :: QBE.LoadType -> MEM.Address -> Exec E.RegVal
readMemory ty addr = do
  mem <- gets envMem
  bytes <- liftIO $ MEM.loadBytes mem addr (QBE.loadByteSize ty)

  case E.fromBytes (QBE.loadToExtType ty) bytes of
    Just x -> pure x
    Nothing -> throwError InvalidMemoryLoad

maybeLookup :: Maybe E.RegVal -> Exec E.RegVal
maybeLookup (Just x) = pure x
maybeLookup Nothing = throwError UnknownVariable

lookupValue :: QBE.BaseType -> QBE.Value -> Exec E.RegVal
lookupValue ty (QBE.VConst (QBE.Const (QBE.Number v))) =
  pure $ case ty of
    QBE.Long -> E.VLong v
    QBE.Word -> E.VWord $ fromIntegral v
    QBE.Single -> E.VSingle $ fromIntegral v
    QBE.Double -> E.VDouble $ fromIntegral v
lookupValue ty (QBE.VConst (QBE.Const (QBE.SFP v))) =
  liftEither $ E.subType ty (E.VSingle v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.DFP v))) =
  liftEither $ E.subType ty (E.VDouble v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.Global k))) = do
  v <- gets envGlobals >>= maybeLookup . Map.lookup k
  liftEither $ E.subType ty v
lookupValue ty (QBE.VConst (QBE.Thread k)) = do
  v <- gets envGlobals >>= maybeLookup . Map.lookup k
  liftEither $ E.subType ty v
lookupValue ty (QBE.VLocal k) = do
  v <- activeFrame >>= maybeLookup . flip lookupLocal k
  liftEither $ E.subType ty v

lookupFunc :: QBE.Value -> Exec QBE.FuncDef
lookupFunc (QBE.VConst (QBE.Const (QBE.Global name))) = do
  funcs <- gets envFuncs
  case Map.lookup name funcs of
    Just def -> pure def
    Nothing -> throwError UnknownFunction
lookupFunc _ = error "non-global functions not supported"

lookupParam :: QBE.FuncParam -> Exec (QBE.LocalIdent, E.RegVal)
lookupParam (QBE.Regular abity ident) = do
  value <- lookupValue (QBE.abityToBase abity) (QBE.VLocal ident)
  return (ident, value)
lookupParam (QBE.Env _) = error "env function parameters not supported"
lookupParam QBE.Variadic = error "variadic functions not supported"

lookupParams :: [QBE.FuncParam] -> Exec RegMap
lookupParams params =
  mapM lookupParam params <&> Map.fromList
