module Language.QBE.Simulator.State where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, modify, put)
import Data.ByteString.Builder qualified as B
import Data.Map qualified as Map
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression
import Language.QBE.Simulator.Memory
import Language.QBE.Types qualified as QBE

type Exec a = StateT Env (ExceptT EvalError IO) a

data StackFrame
  = StackFrame
  { stkFunc :: QBE.FuncDef,
    stkVars :: Map.Map QBE.LocalIdent RegVal,
    stkSize :: Size
  }

mkStackFrame :: QBE.FuncDef -> StackFrame
mkStackFrame func = StackFrame func Map.empty 0

data Env
  = Env
  { envVars :: Map.Map String RegVal,
    envMem :: Memory,
    envStk :: [StackFrame],
    envStkPtr :: Address
  }

mkEnv :: Address -> Size -> IO Env
mkEnv a s = do
  mem <- mkMemory a s
  return $ Env Map.empty mem [] (s - 1)

pushStackFrame :: QBE.FuncDef -> Exec ()
pushStackFrame f =
  modify (\s -> s {envStk = mkStackFrame f : envStk s})

popStackFrame :: Exec (Maybe StackFrame)
popStackFrame = do
  stk <- envStk <$> get
  case stk of
    [] -> pure Nothing
    (x : xs) -> do
      modify (\s -> s {envStk = xs})
      pure $ Just x

pushStack :: Size -> Address -> Exec RegVal
pushStack size align = do
  stkPtr <- envStkPtr <$> get
  let newStkPtr = alignAddr (stkPtr - size) align
  modify (\s -> s {envStkPtr = newStkPtr})
  return $ ELong newStkPtr
  where
    alignAddr :: Address -> Address -> Address
    alignAddr addr alignment = addr - (addr `mod` alignment)

storeValue :: Address -> RegVal -> Exec Env
storeValue addr regVal = do
  env <- get
  liftIO $ storeByteString (envMem env) addr (B.toLazyByteString $ toBuilder regVal)
  pure env

insertValue :: String -> RegVal -> Exec ()
insertValue k v = do
  modify (\s -> s {envVars = Map.insert k v (envVars s)})

lookupValue' :: String -> Exec RegVal
lookupValue' k = do
  e <- get
  case Map.lookup k (envVars e) of
    Nothing -> throwError UnknownVariable
    Just v -> pure v

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
  lookupValue' (show k)
lookupValue _ (QBE.VConst (QBE.Thread k)) =
  lookupValue' (show k)
lookupValue _ (QBE.VLocal k) =
  lookupValue' (show k)
