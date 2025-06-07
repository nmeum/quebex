module Language.QBE.Simulator
  ( Env (envVars, envMem, envStkPtr),
    execInstr,
    execVolatile,
    execStmt,
    execBlock,
    -- execFunc,
    runExec,
  )
where

import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression
import Language.QBE.Simulator.Memory
import Language.QBE.Types qualified as QBE

-- TODO: Turns this into a Monad or use lenses or something?
data Env
  = Env
  { envVars :: Map.Map String RegVal,
    envMem :: Memory,
    envStkPtr :: Address
  }

mkEnv :: Address -> Size -> IO Env
mkEnv a s = do
  mem <- mkMemory a s
  return $ Env Map.empty mem (s - 1)

pushStack :: Size -> Address -> Exec RegVal
pushStack size align = do
  (Env vars mem stkPtr) <- get
  let newStkPtr = alignAddr (stkPtr - size) align
  put (Env vars mem newStkPtr)
  return $ ELong newStkPtr
  where
    alignAddr :: Address -> Address -> Address
    alignAddr addr alignment = addr - (addr `mod` alignment)

insertValue :: String -> RegVal -> Exec Env
insertValue k v = do
  (Env vars mem ptr) <- get
  return $ Env (Map.insert k v vars) mem ptr

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

type Exec a = StateT Env (ExceptT EvalError IO) a

execVolatile :: QBE.VolatileInstr -> Exec Env
execVolatile = error "execVolatile not implemented"

execInstr :: QBE.BaseType -> QBE.Instr -> Exec RegVal
execInstr retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  liftEither (addVals v1 v2) >>= liftEither . assertType retTy
execInstr retTy (QBE.Sub lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  liftEither (subVals v1 v2) >>= liftEither . assertType retTy
execInstr _retTy (QBE.Alloc size align) =
  pushStack (fromIntegral $ QBE.getSize size) align

execStmt :: QBE.Statement -> Exec Env
execStmt (QBE.Assign name ty inst) = do
  rv <- execInstr ty inst
  newEnv <- insertValue (show name) rv
  put newEnv >> pure newEnv
execStmt (QBE.Volatile v) = execVolatile v

execBlock :: QBE.Block -> Exec Env
execBlock block = go $ fmap execStmt (QBE.stmt block)
  where
    go [] = get
    go [x] = x
    go (x : xs) = x >> go xs

-- execFunc :: QBE.FuncDef -> Exec Env
-- execFunc f =

runExec :: Exec Env -> IO (Either EvalError Env)
runExec env = do
  emptyEnv <- liftIO $ mkEnv 0x0 128
  runExceptT (runStateT env emptyEnv) <&> fmap fst
