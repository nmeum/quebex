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

import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put, runStateT)
import Data.Functor ((<&>))
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression
import Language.QBE.Simulator.State
import Language.QBE.Types qualified as QBE

execVolatile :: QBE.VolatileInstr -> Exec Env
execVolatile (QBE.Store valTy valReg addrReg) = do
  -- Since halfwords and bytes are not first class in the IL, storeh and storeb
  -- take a word as argument. Only the first 16 or 8 bits of this word will be
  -- stored in memory at the address specified in the second argument.
  val <- case valTy of
    QBE.Byte -> lookupValue QBE.Word valReg -- TODO: Extract first 8 bits
    QBE.HalfWord -> lookupValue QBE.Word valReg -- TODO: Extract first 16 bits
    (QBE.Base bt) -> lookupValue bt valReg

  -- TODO
  (ELong addrVal) <- lookupValue QBE.Long addrReg
  storeValue addrVal val
execVolatile (QBE.Blit _ _ _) = error "blit not implemented"

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
