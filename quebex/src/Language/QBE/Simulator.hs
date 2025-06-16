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
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Language.QBE (Program, globalFuncs)
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.State
import Language.QBE.Types qualified as QBE

-- Execution of a BasicBlock can either return (with an optional return
-- value) or it can jump to another BasicBlock which will then be executed.
type BlockResult = (Either (Maybe E.RegVal) QBE.Block)

------------------------------------------------------------------------

execVolatile :: QBE.VolatileInstr -> Exec ()
execVolatile (QBE.Store valTy valReg addrReg) = do
  -- Since halfwords and bytes are not first class in the IL, storeh and storeb
  -- take a word as argument. Only the first 16 or 8 bits of this word will be
  -- stored in memory at the address specified in the second argument.
  val <- case valTy of
    QBE.Byte -> do
      (E.VWord v) <- lookupValue QBE.Word valReg
      pure $ E.VByte (fromIntegral v)
    QBE.HalfWord -> do
      (E.VWord v) <- lookupValue QBE.Word valReg
      pure $ E.VHalf (fromIntegral v)
    (QBE.Base bt) -> lookupValue bt valReg

  (E.VLong addrVal) <- lookupValue QBE.Long addrReg
  writeMemory addrVal val
-- TODO: Implement blit
execVolatile (QBE.Blit {}) = error "blit not implemented"

execInstr :: QBE.BaseType -> QBE.Instr -> Exec E.RegVal
execInstr retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  liftEither $ v1 `E.add` v2
execInstr retTy (QBE.Sub lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  liftEither $ v1 `E.sub` v2
execInstr retTy (QBE.Load ty addr) = do
  (E.VLong addrVal) <- lookupValue QBE.Long addr
  val <- readMemory ty addrVal
  ret <- case ty of
    QBE.LSubWord t -> liftEither $ E.extSubWord t val
    QBE.LBase _ -> pure val
  liftEither $ E.subType retTy ret
execInstr QBE.Long (QBE.Alloc size align) =
  stackAlloc (fromIntegral $ QBE.getSize size) align
execInstr _ QBE.Alloc {} = throwError InvalidAddressType

execStmt :: QBE.Statement -> Exec ()
execStmt (QBE.Assign name ty inst) = do
  newVal <- execInstr ty inst
  modifyFrame (storeLocal name newVal)
execStmt (QBE.Volatile v) = execVolatile v
execStmt (QBE.Call ret toCall params) = do
  funcDef <- lookupFunc toCall
  -- TODO: Check if provided args match FuncDef
  funcArgs <- lookupParams params

  mayRetVal <- execFunc funcDef funcArgs
  case mayRetVal of
    Nothing ->
      -- XXX: Could also check funcDef for the return value.
      if isNothing ret
        then pure ()
        else throwError FunctionReturnIgnored
    Just retVal ->
      case ret of
        Nothing -> throwError AssignedVoidReturnValue
        Just (ident, abity) -> do
          let baseTy = QBE.abityToBase abity
          subTyped <- liftEither $ E.subType baseTy retVal
          modifyFrame (storeLocal ident subTyped)

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

execFunc :: QBE.FuncDef -> RegMap -> Exec (Maybe E.RegVal)
execFunc (QBE.FuncDef {QBE.fBlock = []}) _ = pure Nothing
execFunc func@(QBE.FuncDef {QBE.fBlock = block : _}) params = do
  pushStackFrame func
  modifyFrame (pushParams params)

  blockResult <- (execBlock block >>= go) <* popStackFrame
  case blockResult of
    Right _block -> throwError MissingFunctionReturn
    Left maybeValue -> pure maybeValue
  where
    go :: BlockResult -> Exec BlockResult
    go retValue@(Left _) = pure retValue
    go (Right nextBlock) = execBlock nextBlock

    pushParams :: RegMap -> StackFrame -> StackFrame
    pushParams p s@(StackFrame {stkVars = v}) =
      s {stkVars = Map.union p v}

runExec :: Program -> Exec a -> IO (Either EvalError a)
runExec prog env = do
  emptyEnv <- liftIO $ mkEnv (globalFuncs prog) 0x0 128
  runExceptT (runStateT env emptyEnv) <&> fmap fst
