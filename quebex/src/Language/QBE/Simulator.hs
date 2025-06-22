-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

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

import Control.Monad (when)
import Control.Monad.Except (runExceptT, throwError)
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
import Language.QBE.Simulator.Tracer qualified as T
import Language.QBE.Types qualified as QBE

-- Execution of a BasicBlock can either return (with an optional return
-- value) or it can jump to another BasicBlock which will then be executed.
type BlockResult v = (Either (Maybe v) QBE.Block)

------------------------------------------------------------------------

execVolatile :: (T.Tracer t v, E.Storable v, E.ValueRepr v) => QBE.VolatileInstr -> Exec v t ()
execVolatile (QBE.Store valTy valReg addrReg) = do
  -- Since byte and half are not first-class types in the IL, they are
  -- stored as words and have to be looked up as such.
  val <- case valTy of
    QBE.Byte -> lookupValue QBE.Word valReg
    QBE.HalfWord -> lookupValue QBE.Word valReg
    (QBE.Base bt) -> lookupValue bt valReg

  addrVal <- lookupValue QBE.Long addrReg
  toAddressE addrVal >>= (\a -> writeMemory a valTy val)
-- TODO: Implement blit
execVolatile (QBE.Blit {}) = error "blit not implemented"

execInstr :: (T.Tracer t v, E.Storable v, E.ValueRepr v) => QBE.BaseType -> QBE.Instr -> Exec v t v
execInstr retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  runBinary retTy E.add v1 v2
execInstr retTy (QBE.Sub lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  runBinary retTy E.sub v1 v2
execInstr retTy (QBE.Load ty addr) = do
  addrVal <- lookupValue QBE.Long addr

  val <- toAddressE addrVal >>= readMemory ty
  ret <- case ty of
    QBE.LSubWord t -> extendE t val
    QBE.LBase _ -> pure val
  subTypeE retTy ret
execInstr QBE.Long (QBE.Alloc size align) =
  stackAlloc (fromIntegral $ QBE.getSize size) align
execInstr _ QBE.Alloc {} = throwError InvalidAddressType

execStmt :: (T.Tracer t v, E.Storable v, E.ValueRepr v) => QBE.Statement -> Exec v t ()
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
          subTyped <- subTypeE baseTy retVal
          modifyFrame (storeLocal ident subTyped)

execJump :: (T.Tracer t v, E.ValueRepr v) => QBE.JumpInstr -> Exec v t (BlockResult v)
execJump QBE.Halt = throwError EncounteredHalt
execJump (QBE.Jump ident) = do
  blocks <- QBE.fBlock <$> (activeFrame <&> stkFunc)
  case find (\x -> QBE.label x == ident) blocks of
    Just bl -> pure $ Right bl
    Nothing -> throwError (UnknownBlock ident)
execJump (QBE.Jnz cond ifT ifF) = do
  -- TODO: subtyping
  condValue <- lookupValue QBE.Word cond
  let condResult = not $ E.isZero condValue

  trackBranch condValue condResult
  execJump $ QBE.Jump (if condResult then ifT else ifF)
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

execBlock :: (T.Tracer t v, E.Storable v, E.ValueRepr v) => QBE.Block -> Exec v t (BlockResult v)
execBlock block = do
  mapM_ execStmt (QBE.stmt block)
  execJump (QBE.term block)

execFunc :: (T.Tracer t v, E.Storable v, E.ValueRepr v) => QBE.FuncDef -> [v] -> Exec v t (Maybe v)
execFunc (QBE.FuncDef {QBE.fBlock = []}) _ = pure Nothing
execFunc func@(QBE.FuncDef {QBE.fBlock = block : _, QBE.fParams = params}) args = do
  when (length params /= length args) $
    throwError (FuncArgsMismatch $ QBE.fName func)
  pushStackFrame func

  let vars = Map.fromList $ zip (map paramName params) args
  modifyFrame (\s -> s {stkVars = vars})

  blockResult <- (execBlock block >>= go) <* popStackFrame
  case blockResult of
    Right _block -> throwError MissingFunctionReturn
    Left maybeValue -> pure maybeValue
  where
    go :: (T.Tracer t v, E.Storable v, E.ValueRepr v) => BlockResult v -> Exec v t (BlockResult v)
    go retValue@(Left _) = pure retValue
    go (Right nextBlock) = execBlock nextBlock >>= go

    paramName :: QBE.FuncParam -> QBE.LocalIdent
    paramName (QBE.Regular _ n) = n
    paramName (QBE.Env n) = n
    paramName QBE.Variadic = error "variadic parameters not supported"

runExec :: (T.Tracer t v, E.ValueRepr v) => Program -> Exec v t a -> t -> IO (Either EvalError a)
runExec prog env tracer = do
  emptyEnv <- liftIO $ mkEnv (globalFuncs prog) 0x0 128 tracer
  runExceptT (runStateT env emptyEnv) <&> fmap fst
