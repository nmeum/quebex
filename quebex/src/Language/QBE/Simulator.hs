module Language.QBE.Simulator
  ( Env (envMem, envStkPtr),
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
import Control.Monad.State (get, runStateT)
import Data.Functor ((<&>))
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression
import Language.QBE.Simulator.State
import Language.QBE.Types qualified as QBE

execVolatile :: QBE.VolatileInstr -> Exec ()
execVolatile (QBE.Store valTy valReg addrReg) = do
  -- Since halfwords and bytes are not first class in the IL, storeh and storeb
  -- take a word as argument. Only the first 16 or 8 bits of this word will be
  -- stored in memory at the address specified in the second argument.
  val <- case valTy of
    QBE.Byte -> lookupValue QBE.Word valReg -- TODO: Extract first 8 bits
    QBE.HalfWord -> lookupValue QBE.Word valReg -- TODO: Extract first 16 bits
    (QBE.Base bt) -> lookupValue bt valReg

  (ELong addrVal) <- lookupValue QBE.Long addrReg
  writeMemory addrVal val
execVolatile (QBE.Blit {}) = error "blit not implemented"

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
  stackAlloc (fromIntegral $ QBE.getSize size) align

execStmt :: QBE.Statement -> Exec ()
execStmt (QBE.Assign name ty inst) = do
  newVal <- execInstr ty inst
  modifyFrame (storeLocal name newVal)
execStmt (QBE.Volatile v) = execVolatile v

execBlock :: QBE.Block -> Exec ()
execBlock block = mapM_ execStmt (QBE.stmt block)

-- execFunc :: QBE.FuncDef -> Exec Env
-- execFunc f =

runExec :: Exec () -> IO (Either EvalError Env)
runExec env = do
  emptyEnv <- liftIO $ mkEnv 0x0 128
  runExceptT (runStateT (env >> get) emptyEnv) <&> fmap fst
