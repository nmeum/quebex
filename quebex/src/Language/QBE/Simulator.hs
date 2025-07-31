-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator
  ( BlockResult,
    execInstr,
    execVolatile,
    execStmt,
    execBlock,
    execFunc,
  )
where

import Control.Monad (void, when)
import Control.Monad.Catch (throwM)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Word (Word8)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.State
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory (addrOverlap)
import Language.QBE.Simulator.State
import Language.QBE.Types qualified as QBE

-- Execution of a BasicBlock can either return (with an optional return
-- value) or it can jump to another BasicBlock which will then be executed.
type BlockResult v = (Either (Maybe v) QBE.Block)

------------------------------------------------------------------------

execVolatile :: (Simulator m v) => QBE.VolatileInstr -> m ()
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
    throwM $
      OverlappingBlit srcAddr dstAddr

  -- Somehow allow specialization of memory copies, e.g. for quebex-symex.
  when (toCopy > 0) $
    mapM_
      ( \off -> do
          srcByte <- readMemory (QBE.LSubWord QBE.UnsignedByte) (srcAddr + off)
          writeMemory (dstAddr + off) QBE.Byte srcByte
      )
      [0 .. toCopy - 1]
{-# INLINEABLE execVolatile #-}

execBinary ::
  (Simulator m v) =>
  QBE.BaseType ->
  (v -> v -> Maybe v) ->
  QBE.Value ->
  QBE.Value ->
  m v
execBinary retTy op lhs rhs = do
  v1 <- lookupValue retTy lhs
  v2 <- lookupValue retTy rhs
  runBinary retTy op v1 v2

execInstr :: (Simulator m v) => QBE.BaseType -> QBE.Instr -> m v
execInstr retTy (QBE.Add lhs rhs) = execBinary retTy E.add lhs rhs
execInstr retTy (QBE.Sub lhs rhs) = execBinary retTy E.sub lhs rhs
execInstr retTy (QBE.Mul lhs rhs) = execBinary retTy E.mul lhs rhs
execInstr retTy (QBE.Or lhs rhs) = execBinary retTy E.or lhs rhs
execInstr retTy (QBE.Xor lhs rhs) = execBinary retTy E.xor lhs rhs
execInstr retTy (QBE.And lhs rhs) = execBinary retTy E.and lhs rhs
execInstr retTy (QBE.URem lhs rhs) = execBinary retTy E.urem lhs rhs
execInstr retTy (QBE.Rem lhs rhs) = execBinary retTy E.srem lhs rhs
execInstr retTy (QBE.UDiv lhs rhs) = execBinary retTy E.udiv lhs rhs
execInstr retTy (QBE.Load ty addr) = do
  addrVal <- lookupValue QBE.Long addr

  val <- toAddressE addrVal >>= readMemory ty
  subTypeE retTy val
execInstr QBE.Long (QBE.Alloc align sizeValue) = do
  size <- lookupValue QBE.Long sizeValue >>= toAddressE
  stackAlloc size (fromIntegral $ QBE.getSize align)
execInstr _ QBE.Alloc {} = throwM InvalidAddressType
execInstr retTy (QBE.Compare cmpTy cmpOp lhs rhs) = do
  v1 <- lookupValue cmpTy lhs
  v2 <- lookupValue cmpTy rhs

  let exprOp = E.compareExpr cmpOp
  runBinary retTy exprOp v1 v2
execInstr retTy (QBE.Ext subLongTy value) = do
  v <- lookupValue QBE.Word value
  wordToLongE subLongTy v >>= subTypeE retTy
{-# INLINEABLE execInstr #-}

execStmt :: (Simulator m v) => QBE.Statement -> m ()
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
        else throwM FunctionReturnIgnored
    Just retVal ->
      case ret of
        Nothing -> throwM AssignedVoidReturnValue
        Just (ident, abity) -> do
          let baseTy = QBE.abityToBase abity
          subTyped <- subTypeE baseTy retVal
          modifyFrame (storeLocal ident subTyped)
{-# INLINEABLE execStmt #-}

execJump :: (Simulator m v) => QBE.JumpInstr -> m (BlockResult v)
execJump QBE.Halt = throwM EncounteredHalt
execJump (QBE.Jump ident) = do
  blocks <- QBE.fBlock <$> (activeFrame <&> stkFunc)
  case find (\x -> QBE.label x == ident) blocks of
    Just bl -> pure $ Right bl
    Nothing -> throwM (UnknownBlock ident)
execJump (QBE.Jnz cond ifT ifF) = do
  condValue <- lookupValue QBE.Word cond
  let condResult = not $ E.isZero condValue

  condBranch condValue condResult
  execJump $ QBE.Jump (if condResult then ifT else ifF)
execJump (QBE.Return v) = do
  func <- activeFrame <&> stkFunc
  case QBE.fAbity func of
    Just abity -> do
      retVal <-
        case v of
          Nothing -> throwM InvalidReturnValue
          Just x -> pure x
      lookupValue (QBE.abityToBase abity) retVal <&> (Left . Just)
    Nothing ->
      if isNothing v
        then pure (Left Nothing)
        else throwM InvalidReturnValue
{-# INLINEABLE execJump #-}

execPhi :: (Simulator m v) => Maybe QBE.BlockIdent -> QBE.Phi -> m ()
execPhi Nothing _ = throwM InvalidPhiPosition
execPhi (Just prevIdent) (QBE.Phi name ty labels) =
  case Map.lookup prevIdent labels of
    Nothing -> throwM (UnknownPhiLabel prevIdent)
    Just v -> do
      retVal <- lookupValue ty v
      modifyFrame (storeLocal name retVal)
{-# INLINEABLE execPhi #-}

execBlock :: (Simulator m v) => Maybe QBE.BlockIdent -> QBE.Block -> m (BlockResult v)
execBlock prevIdent block = do
  mapM_ (execPhi prevIdent) (QBE.phi block)
  mapM_ execStmt (QBE.stmt block)
  execJump (QBE.term block)
{-# INLINEABLE execBlock #-}

execFunc :: (Simulator m v) => QBE.FuncDef -> [v] -> m (Maybe v)
execFunc (QBE.FuncDef {QBE.fBlock = []}) _ = pure Nothing
execFunc func@(QBE.FuncDef {QBE.fBlock = block : _, QBE.fParams = params}) args = do
  when (length params /= length args) $
    throwM (FuncArgsMismatch $ QBE.fName func)
  void $ newStackFrame func

  let vars = Map.fromList $ zip (map paramName params) args
  modifyFrame (\s -> s {stkVars = vars})

  blockResult <- (execBlock Nothing block >>= go Nothing) <* returnFromFunc
  case blockResult of
    Right _block -> throwM MissingFunctionReturn
    Left maybeValue -> pure maybeValue
  where
    go :: (Simulator m v) => Maybe QBE.BlockIdent -> BlockResult v -> m (BlockResult v)
    go _ retValue@(Left _) = pure retValue
    go prevIdent (Right nextBlock) =
      execBlock prevIdent nextBlock >>= go (Just $ QBE.label nextBlock)

    paramName :: QBE.FuncParam -> QBE.LocalIdent
    paramName (QBE.Regular _ n) = n
    paramName (QBE.Env n) = n
    paramName QBE.Variadic = error "variadic parameters not supported"
{-# SPECIALIZE execFunc :: QBE.FuncDef -> [DE.RegVal] -> SimState DE.RegVal Word8 (Maybe DE.RegVal) #-}
