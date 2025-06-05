module Language.QBE.Simulator
  ( execInstr,
    execVolatile,
    execStmt,
    execBlock,
    -- execFunc,
    runExec,
  )
where

import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression
import Language.QBE.Types qualified as QBE

type Env = Map.Map String RegVal

lookupValue' :: String -> Exec RegVal
lookupValue' k = do
  env <- get
  case Map.lookup k env of
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
execInstr _retTy (QBE.Alloc _size _align) = do
  -- TODO: This needs a byte-addressable memory implementation
  -- needs to return a pointer to the allocated address in memory.
  error "alloc not yet implemented"

execStmt :: QBE.Statement -> Exec Env
execStmt (QBE.Assign name ty inst) = do
  rv <- execInstr ty inst
  newEnv <- get <&> Map.insert (show name) rv
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
runExec env =
  runExceptT (runStateT env Map.empty) <&> fmap fst
