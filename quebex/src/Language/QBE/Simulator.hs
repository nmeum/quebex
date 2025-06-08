module Language.QBE.Simulator
  ( Env (envMem, envStkPtr),
    execInstr,
    execVolatile,
    execStmt,
    execBlock,
    execFunc,
    runExec,
  )
where

import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, runStateT)
import Data.Functor ((<&>))
import Language.QBE.Simulator.Error
import qualified Language.QBE.Simulator.Expression as E
import Language.QBE.Simulator.State
import Language.QBE.Types qualified as QBE

checkedEval ::
  QBE.BaseType ->
  (E.RegVal -> E.RegVal -> Either EvalError E.RegVal) ->
  E.RegVal ->
  E.RegVal ->
  Exec E.RegVal
checkedEval ty op lhs rhs = liftEither $ E.checkedEval ty op lhs rhs

------------------------------------------------------------------------

execVolatile :: QBE.VolatileInstr -> Exec ()
execVolatile (QBE.Store valTy valReg addrReg) = do
  -- Since halfwords and bytes are not first class in the IL, storeh and storeb
  -- take a word as argument. Only the first 16 or 8 bits of this word will be
  -- stored in memory at the address specified in the second argument.
  val <- case valTy of
    QBE.Byte -> lookupValue QBE.Word valReg -- TODO: Extract first 8 bits
    QBE.HalfWord -> lookupValue QBE.Word valReg -- TODO: Extract first 16 bits
    (QBE.Base bt) -> lookupValue bt valReg

  (E.VLong addrVal) <- lookupValue QBE.Long addrReg
  writeMemory addrVal val
execVolatile (QBE.Blit {}) = error "blit not implemented"

execInstr :: QBE.BaseType -> QBE.Instr -> Exec E.RegVal
execInstr retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  checkedEval retTy E.add v1 v2
execInstr retTy (QBE.Sub lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  checkedEval retTy E.sub v1 v2
execInstr _retTy (QBE.Alloc size align) =
  stackAlloc (fromIntegral $ QBE.getSize size) align

execStmt :: QBE.Statement -> Exec ()
execStmt (QBE.Assign name ty inst) = do
  newVal <- execInstr ty inst
  modifyFrame (storeLocal name newVal)
execStmt (QBE.Volatile v) = execVolatile v

execBlock :: QBE.Block -> Exec ()
-- TODO: Handle terms.
execBlock block = mapM_ execStmt (QBE.stmt block)

execFunc :: QBE.FuncDef -> Exec ()
execFunc func = do
  pushStackFrame func
  -- TODO: push function arguments on the stack
  mapM_ execBlock (QBE.fBlock func)
  -- TODO: return function return value

runExec :: Exec () -> IO (Either EvalError Env)
runExec env = do
  emptyEnv <- liftIO $ mkEnv 0x0 128
  runExceptT (runStateT (env >> get) emptyEnv) <&> fmap fst
