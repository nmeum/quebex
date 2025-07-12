-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# LANGUAGE FunctionalDependencies #-}

module Language.QBE.Simulator.State where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE

data StackFrame v
  = StackFrame
  { stkFunc :: QBE.FuncDef,
    stkVars :: Map.Map QBE.LocalIdent v,
    stkFp :: E.Address
  }

mkStackFrame :: QBE.FuncDef -> E.Address -> StackFrame v
mkStackFrame func = StackFrame func Map.empty

storeLocal :: QBE.LocalIdent -> v -> StackFrame v -> StackFrame v
storeLocal ident value frame@(StackFrame {stkVars = v}) =
  frame {stkVars = Map.insert ident value v}

lookupLocal :: StackFrame v -> QBE.LocalIdent -> Maybe v
lookupLocal (StackFrame {stkVars = v}) = flip Map.lookup v
{-# INLINEABLE lookupLocal #-}

------------------------------------------------------------------------

-- This is an “abstract monad” representing the Simulator and allowing
-- interaction with an encapsulated Simulator state 'm'. The module
-- 'Language.QBE.Simulator.Default.State' provides an implementation of
-- a polymorphic Simulator state implement over a State monad.
--
-- The idea is inspired by Bourgeat et al. https://doi.org/10.1145/3607833
class (E.ValueRepr v, MonadThrow m) => Simulator m v | m -> v where
  condBranch :: v -> Bool -> m ()

  lookupGlobal :: QBE.GlobalIdent -> m (Maybe v)
  findFunc :: QBE.GlobalIdent -> m (Maybe QBE.FuncDef)

  activeFrame :: m (StackFrame v)
  pushStackFrame :: StackFrame v -> m ()
  popStackFrame :: m (StackFrame v)
  getSP :: m E.Address
  setSP :: E.Address -> m ()

  writeMemory :: E.Address -> QBE.ExtType -> v -> m () -- TODO: LoadType?
  readMemory :: QBE.LoadType -> E.Address -> m v

liftMaybe :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
liftMaybe e Nothing = throwM e
liftMaybe _ (Just r) = pure r
{-# INLINE liftMaybe #-}

toAddressE :: (Simulator m v) => v -> m E.Address
toAddressE = liftMaybe InvalidAddressType . E.toAddress
{-# INLINEABLE toAddressE #-}

subTypeE :: (Simulator m v) => QBE.BaseType -> v -> m v
subTypeE ty v = liftMaybe TypingError $ E.subType ty v
{-# INLINEABLE subTypeE #-}

wordToLongE :: (Simulator m v) => QBE.SubLongType -> v -> m v
wordToLongE ty v = liftMaybe InvaldSubWordExtension $ E.wordToLong ty v
{-# INLINEABLE wordToLongE #-}

runBinary ::
  (Simulator m v) =>
  QBE.BaseType ->
  (v -> v -> Maybe v) ->
  v ->
  v ->
  m v
runBinary ty op lhs rhs = do
  lhs' <- subTypeE ty lhs
  rhs' <- subTypeE ty rhs
  liftMaybe TypingError (op lhs' rhs')
{-# INLINEABLE runBinary #-}

modifyFrame :: (Simulator m v) => (StackFrame v -> StackFrame v) -> m ()
modifyFrame func = do
  frame <- popStackFrame
  pushStackFrame (func frame)
{-# INLINEABLE modifyFrame #-}

stackAlloc :: (Simulator m v) => MEM.Size -> E.Address -> m v
stackAlloc size align = do
  stkPtr <- getSP
  let newStkPtr = alignAddr (stkPtr - size) align
  setSP newStkPtr

  return $ E.fromAddress newStkPtr
  where
    alignAddr :: E.Address -> E.Address -> E.Address
    alignAddr addr alignment = addr - (addr `mod` alignment)
{-# INLINEABLE stackAlloc #-}

newStackFrame :: (Simulator m v) => QBE.FuncDef -> m (StackFrame v)
newStackFrame f = do
  frame <- getSP <&> mkStackFrame f
  pushStackFrame frame >> pure frame
{-# INLINEABLE newStackFrame #-}

returnFromFunc :: (Simulator m v) => m ()
returnFromFunc = popStackFrame >>= setSP . stkFp
{-# INLINE returnFromFunc #-}

maybeLookup :: (Simulator m v) => String -> Maybe v -> m v
maybeLookup name = liftMaybe (UnknownVariable name)
{-# INLINE maybeLookup #-}

lookupValue :: (Simulator m v) => QBE.BaseType -> QBE.Value -> m v
lookupValue ty (QBE.VConst (QBE.Const (QBE.Number v))) =
  pure $ E.fromLit ty v
lookupValue ty (QBE.VConst (QBE.Const (QBE.SFP v))) =
  subTypeE ty (E.fromFloat v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.DFP v))) =
  subTypeE ty (E.fromDouble v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.Global k))) = do
  v <- lookupGlobal k >>= maybeLookup (show k)
  subTypeE ty v
lookupValue ty (QBE.VConst (QBE.Thread k)) = do
  v <- lookupGlobal k >>= maybeLookup (show k)
  subTypeE ty v
lookupValue ty (QBE.VLocal k) = do
  v <- activeFrame >>= maybeLookup (show k) . flip lookupLocal k
  subTypeE ty v
{-# INLINEABLE lookupValue #-}

lookupFunc :: (Simulator m v) => QBE.Value -> m QBE.FuncDef
lookupFunc (QBE.VConst (QBE.Const (QBE.Global name))) = do
  maybeFunc <- findFunc name
  case maybeFunc of
    Just def -> pure def
    Nothing -> throwM (UnknownFunction name)
lookupFunc _ = error "non-global functions not supported"
{-# INLINEABLE lookupFunc #-}

lookupArg :: (Simulator m v) => QBE.FuncArg -> m v
lookupArg (QBE.ArgReg abity value) = do
  lookupValue (QBE.abityToBase abity) value
lookupArg (QBE.ArgEnv _) = error "env function parameters not supported"
lookupArg QBE.ArgVar = error "variadic functions not supported"
{-# INLINEABLE lookupArg #-}

lookupArgs :: (Simulator m v) => [QBE.FuncArg] -> m [v]
lookupArgs = mapM lookupArg
{-# INLINE lookupArgs #-}
