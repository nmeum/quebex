module Language.QBE.Simulator
  ( Env (envMem, envStkPtr),
    BlockResult,
    execInstr,
    execVolatile,
    execStmt,
    execBlock,
    execFunc,
    runExec,
  )
where

import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (runStateT)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (isNothing)
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.State
import Language.QBE.Types qualified as QBE

-- Execution of a BasicBlock can either return (with an optional return
-- value) or it can jump to another BasicBlock which will then be executed.
type BlockResult = (Either (Maybe E.RegVal) QBE.Block)

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
-- TODO: Implement blit
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
execInstr _retTy (QBE.Load ty addr) = do
  (E.VLong addrVal) <- lookupValue QBE.Long addr
  -- TODO: Respect specified return type
  -- TODO: Sign and zero extension for SubWordType
  readMemory ty addrVal
execInstr _retTy (QBE.Alloc size align) =
  -- TODO: Ensure that _retTy is a long?
  stackAlloc (fromIntegral $ QBE.getSize size) align

execStmt :: QBE.Statement -> Exec ()
execStmt (QBE.Assign name ty inst) = do
  newVal <- execInstr ty inst
  modifyFrame (storeLocal name newVal)
execStmt (QBE.Volatile v) = execVolatile v

execJump :: QBE.JumpInstr -> Exec BlockResult
execJump QBE.Halt = throwError EncounteredHalt
execJump (QBE.Jump ident) = do
  blocks <- QBE.fBlock <$> (activeFrame <&> stkFunc)
  case find (\x -> QBE.label x == ident) blocks of
    Just bl -> pure $ Right bl
    Nothing -> throwError UnknownBlock
execJump (QBE.Jnz cond ifT ifF) = do
  (E.VWord condValue) <- lookupValue QBE.Word cond
  execJump $ QBE.Jump (if condValue /= 0 then ifT else ifF)
execJump (QBE.Return v) = do
  func <- activeFrame <&> stkFunc
  case QBE.fAbity func of
    Just abity -> do
      retVal <-
        case v of
          Nothing -> throwError InvalidReturnValue
          Just x -> pure x
      lookupValue (QBE.abityToBase abity) retVal <&> (Left . Just)
    Nothing ->
      if isNothing v
        then pure (Left Nothing)
        else throwError InvalidReturnValue

execBlock :: QBE.Block -> Exec BlockResult
execBlock block = do
  mapM_ execStmt (QBE.stmt block)
  execJump (QBE.term block)

execFunc :: QBE.FuncDef -> Exec BlockResult
execFunc (QBE.FuncDef {QBE.fBlock = []}) = pure (Left Nothing)
execFunc func@(QBE.FuncDef {QBE.fBlock = block : _}) = do
  pushStackFrame func
  -- TODO: push function arguments on the stack
  (execBlock block >>= go) <* popStackFrame
  where
    go :: BlockResult -> Exec BlockResult
    go retValue@(Left _) = pure retValue
    go (Right nextBlock) = execBlock nextBlock

runExec :: Exec a -> IO (Either EvalError a)
runExec env = do
  emptyEnv <- liftIO $ mkEnv 0x0 128
  runExceptT (runStateT env emptyEnv) <&> fmap fst
