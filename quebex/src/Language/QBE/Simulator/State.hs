module Language.QBE.Simulator.State where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, modify)
import Data.ByteString.Builder qualified as B
import Data.Map qualified as Map
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression
import Language.QBE.Simulator.Memory
import Language.QBE.Types qualified as QBE

type Exec a = StateT Env (ExceptT EvalError IO) a

------------------------------------------------------------------------

data StackFrame
  = StackFrame
  { stkFunc :: QBE.FuncDef,
    stkVars :: Map.Map QBE.LocalIdent RegVal,
    stkFp :: Address
  }

mkStackFrame :: QBE.FuncDef -> Address -> StackFrame
mkStackFrame func = StackFrame func Map.empty

storeLocal :: QBE.LocalIdent -> RegVal -> StackFrame -> StackFrame
storeLocal ident value frame@(StackFrame {stkVars = v}) =
  frame {stkVars = Map.insert ident value v}

lookupLocal :: StackFrame -> QBE.LocalIdent -> Maybe RegVal
lookupLocal (StackFrame {stkVars = v}) = flip Map.lookup v

------------------------------------------------------------------------

data Env
  = Env
  { envGlobals :: Map.Map QBE.GlobalIdent RegVal,
    envMem :: Memory,
    envStk :: [StackFrame],
    envStkPtr :: Address
  }

mkEnv :: Address -> Size -> IO Env
mkEnv a s = do
  mem <- mkMemory a s
  return $ Env Map.empty mem [] (s - 1)

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

stackAlloc :: Size -> Address -> Exec RegVal
stackAlloc size align = do
  stkPtr <- gets envStkPtr
  let newStkPtr = alignAddr (stkPtr - size) align
  modify (\s -> s {envStkPtr = newStkPtr})
  return $ ELong newStkPtr
  where
    alignAddr :: Address -> Address -> Address
    alignAddr addr alignment = addr - (addr `mod` alignment)

writeMemory :: Address -> RegVal -> Exec ()
writeMemory addr regVal = do
  mem <- gets envMem
  liftIO $ storeByteString mem addr (B.toLazyByteString $ toBuilder regVal)

maybeLookup :: Maybe RegVal -> Exec RegVal
maybeLookup (Just x) = pure x
maybeLookup Nothing = throwError UnknownVariable

lookupValue :: QBE.BaseType -> QBE.Value -> Exec RegVal
lookupValue ty (QBE.VConst (QBE.Const (QBE.Number v))) =
  pure $ case ty of
    QBE.Long -> ELong v
    QBE.Word -> EWord $ fromIntegral v
    QBE.Single -> ESingle $ fromIntegral v
    QBE.Double -> EDouble $ fromIntegral v
lookupValue _ (QBE.VConst (QBE.Const (QBE.SFP v))) = pure $ ESingle v
lookupValue _ (QBE.VConst (QBE.Const (QBE.DFP v))) = pure $ EDouble v
lookupValue _ (QBE.VConst (QBE.Const (QBE.Global k))) =
  gets envGlobals >>= maybeLookup . Map.lookup k
lookupValue _ (QBE.VConst (QBE.Thread k)) =
  gets envGlobals >>= maybeLookup . Map.lookup k
lookupValue _ (QBE.VLocal k) =
  activeFrame >>= maybeLookup . flip lookupLocal k
