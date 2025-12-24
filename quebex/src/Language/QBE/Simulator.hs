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

  addr <- lookupValue QBE.Long addrReg >>= toAddress
  writeMemory addr valTy val
execVolatile (QBE.Blit src dst toCopy) = do
  srcAddrVal <- lookupValue QBE.Long src
  dstAddrVal <- lookupValue QBE.Long dst

  -- TODO: Check for invalid BLITs
  srcAddr <- toAddress srcAddrVal
  dstAddr <- toAddress dstAddrVal
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

execBinaryTy ::
  (Simulator m v) =>
  QBE.BaseType ->
  (v -> v -> Maybe v) ->
  (QBE.BaseType, QBE.Value) ->
  (QBE.BaseType, QBE.Value) ->
  m v
execBinaryTy retTy op (lty, lhs) (rty, rhs) = do
  v1 <- lookupValue lty lhs
  v2 <- lookupValue rty rhs
  runBinary retTy op v1 v2

execBinary ::
  (Simulator m v) =>
  QBE.BaseType ->
  (v -> v -> Maybe v) ->
  QBE.Value ->
  QBE.Value ->
  m v
execBinary retTy op lhs rhs =
  execBinaryTy retTy op (retTy, lhs) (retTy, rhs)
{-# INLINE execBinary #-}

execShift ::
  (Simulator m v) =>
  QBE.BaseType ->
  (v -> v -> Maybe v) ->
  QBE.Value ->
  QBE.Value ->
  m v
execShift retTy op lhs amount =
  execBinaryTy retTy op (retTy, lhs) (QBE.Word, amount)
{-# INLINE execShift #-}

execInstr :: (Simulator m v) => QBE.BaseType -> QBE.Instr -> m v
execInstr retTy (QBE.Neg op) = do
  v <- lookupValue retTy op
  liftMaybe TypingError (E.neg v)
execInstr retTy (QBE.Add lhs rhs) = execBinary retTy E.add lhs rhs
execInstr retTy (QBE.Sub lhs rhs) = execBinary retTy E.sub lhs rhs
execInstr retTy (QBE.Mul lhs rhs) = execBinary retTy E.mul lhs rhs
execInstr retTy (QBE.Div lhs rhs) = execBinary retTy E.div lhs rhs
execInstr retTy (QBE.Or lhs rhs) = execBinary retTy E.or lhs rhs
execInstr retTy (QBE.Xor lhs rhs) = execBinary retTy E.xor lhs rhs
execInstr retTy (QBE.And lhs rhs) = execBinary retTy E.and lhs rhs
execInstr retTy (QBE.URem lhs rhs) = execBinary retTy E.urem lhs rhs
execInstr retTy (QBE.Rem lhs rhs) = execBinary retTy E.srem lhs rhs
execInstr retTy (QBE.UDiv lhs rhs) = execBinary retTy E.udiv lhs rhs
execInstr retTy (QBE.Sar lhs rhs) = execShift retTy E.sar lhs rhs
execInstr retTy (QBE.Shr lhs rhs) = execShift retTy E.shr lhs rhs
execInstr retTy (QBE.Shl lhs rhs) = execShift retTy E.shl lhs rhs
execInstr retTy (QBE.Load ty addrVal) = do
  addr <- lookupValue QBE.Long addrVal >>= toAddress
  val <- readMemory ty addr
  subTypeE retTy val
execInstr QBE.Long (QBE.Alloc align sizeValue) = do
  size <- lookupValue QBE.Long sizeValue
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
execInstr retTy (QBE.Copy value) = do
  lookupValue retTy value
execInstr retTy (QBE.Cast value) = do
  -- We must deduce the value type to use for lookup from
  -- the return type as manadated by the cast type string.
  let valueType =
        case retTy of
          QBE.Word -> QBE.Single
          QBE.Long -> QBE.Double
          QBE.Single -> QBE.Word
          QBE.Double -> QBE.Long

  -- TODO: Consider adding an explicit operation for casting
  -- of floating points to the expression language abstraction.
  v <- lookupValue valueType value
  pure (E.fromLit (QBE.Base retTy) $ E.toWord64 v)
{-# INLINEABLE execInstr #-}

execStmt :: (Simulator m v) => QBE.Statement -> m ()
execStmt (QBE.Assign name ty inst) = do
  newVal <- execInstr ty inst
  modifyFrame (storeLocal name newVal)
execStmt (QBE.Volatile v) = execVolatile v
execStmt (QBE.Call ret toCall params) = do
  function <- lookupFunc toCall
  -- TODO: Check if provided args match FuncDef
  funcArgs <- lookupArgs params

  mayRetVal <- case function of
    SFuncDef funcDef -> execFunc funcDef funcArgs
    SSimFunc simFunc -> simFunc funcArgs

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
  condResult <- isTrue condValue
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
    Nothing -> throwM (UnknownBlock prevIdent)
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
{-# INLINEABLE execFunc #-}
