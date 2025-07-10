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
import Language.QBE.Simulator.Memory (addrOverlap)
import Language.QBE.Simulator.State
import Language.QBE.Simulator.Tracer qualified as T
import Language.QBE.Types qualified as QBE

-- Execution of a BasicBlock can either return (with an optional return
-- value) or it can jump to another BasicBlock which will then be executed.
type BlockResult v = (Either (Maybe v) QBE.Block)

------------------------------------------------------------------------

execVolatile :: (T.Tracer t v, E.Storable v b, E.ValueRepr v) => QBE.VolatileInstr -> Exec v b t ()
execVolatile (QBE.Store valTy valReg addrReg) = do
  -- Since byte and half are not first-class types in the IL, they are
  -- stored as words and have to be looked up as such.
  val <- case valTy of
    QBE.Byte -> lookupValue QBE.Word valReg
    QBE.HalfWord -> lookupValue QBE.Word valReg
    (QBE.Base bt) -> lookupValue bt valReg

  addrVal <- lookupValue QBE.Long addrReg
  toAddressE addrVal >>= (\a -> writeMemory a valTy val)
execVolatile (QBE.Blit src dst toCopy) = do
  srcAddr <- lookupValue QBE.Long src >>= toAddressE
  dstAddr <- lookupValue QBE.Long dst >>= toAddressE

  -- The third argument is the number of bytes to copy. The source and
  -- destination spans are required to be either non-overlapping, or fully
  -- overlapping (source address identical to the destination address).
  when (srcAddr /= dstAddr && addrOverlap srcAddr dstAddr toCopy) $
    throwError $
      OverlappingBlit srcAddr dstAddr

  -- Somehow allow specialization of memory copies, e.g. for quebex-symex.
  when (toCopy > 0) $
    mapM_
      ( \off -> do
          srcByte <- readMemory (QBE.LSubWord QBE.UnsignedByte) (srcAddr + off)
          writeMemory (dstAddr + off) QBE.Byte srcByte
      )
      [0 .. toCopy - 1]

execInstr :: (T.Tracer t v, E.Storable v b, E.ValueRepr v) => QBE.BaseType -> QBE.Instr -> Exec v b t v
execInstr retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  runBinary retTy E.add v1 v2
execInstr retTy (QBE.Sub lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  runBinary retTy E.sub v1 v2
execInstr retTy (QBE.Mul lhs rhs) = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  runBinary retTy E.mul v1 v2
execInstr retTy (QBE.Load ty addr) = do
  addrVal <- lookupValue QBE.Long addr

  val <- toAddressE addrVal >>= readMemory ty
  subTypeE retTy val
execInstr QBE.Long (QBE.Alloc align sizeValue) = do
  size <- lookupValue QBE.Long sizeValue >>= toAddressE
  stackAlloc size (fromIntegral $ QBE.getSize align)
execInstr _ QBE.Alloc {} = throwError InvalidAddressType
execInstr retTy (QBE.Compare cmpTy cmpOp lhs rhs) = do
  v1 <- lookupValue cmpTy lhs
  v2 <- lookupValue cmpTy rhs

  let exprOp = E.compareExpr cmpOp
  runBinary retTy exprOp v1 v2
execInstr retTy (QBE.Ext subLongTy value) = do
  v <- lookupValue QBE.Word value
  wordToLongE subLongTy v >>= subTypeE retTy

execStmt :: (T.Tracer t v, E.Storable v b, E.ValueRepr v) => QBE.Statement -> Exec v b t ()
execStmt (QBE.Assign name ty inst) = do
  newVal <- execInstr ty inst
  modifyFrame (storeLocal name newVal)
execStmt (QBE.Volatile v) = execVolatile v
execStmt (QBE.Call ret toCall params) = do
  funcDef <- lookupFunc toCall
  -- TODO: Check if provided args match FuncDef
  funcArgs <- lookupArgs params

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

execJump :: (T.Tracer t v, E.ValueRepr v) => QBE.JumpInstr -> Exec v b t (BlockResult v)
execJump QBE.Halt = throwError EncounteredHalt
execJump (QBE.Jump ident) = do
  blocks <- QBE.fBlock <$> (activeFrame <&> stkFunc)
  case find (\x -> QBE.label x == ident) blocks of
    Just bl -> pure $ Right bl
    Nothing -> throwError (UnknownBlock ident)
execJump (QBE.Jnz cond ifT ifF) = do
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

execBlock :: (T.Tracer t v, E.Storable v b, E.ValueRepr v) => QBE.Block -> Exec v b t (BlockResult v)
execBlock block = do
  mapM_ execStmt (QBE.stmt block)
  execJump (QBE.term block)

execFunc :: (T.Tracer t v, E.Storable v b, E.ValueRepr v) => QBE.FuncDef -> [v] -> Exec v b t (Maybe v)
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
    go :: (T.Tracer t v, E.Storable v b, E.ValueRepr v) => BlockResult v -> Exec v b t (BlockResult v)
    go retValue@(Left _) = pure retValue
    go (Right nextBlock) = execBlock nextBlock >>= go

    paramName :: QBE.FuncParam -> QBE.LocalIdent
    paramName (QBE.Regular _ n) = n
    paramName (QBE.Env n) = n
    paramName QBE.Variadic = error "variadic parameters not supported"

runExec :: (E.Storable v b, T.Tracer t v, E.ValueRepr v) => Program -> Exec v b t a -> t -> IO (Either EvalError a)
runExec prog env tracer = do
  emptyEnv <- liftIO $ mkEnv (globalFuncs prog) 0x0 (1024 * 1024 * 10) tracer
  runExceptT (runStateT env emptyEnv) <&> fmap fst
