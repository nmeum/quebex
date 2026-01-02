-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# LANGUAGE FunctionalDependencies #-}

module Language.QBE.Simulator.State where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Word (Word64)
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE

data StackFrame v
  = StackFrame
  { stkFunc :: QBE.FuncDef,
    stkVars :: Map.Map QBE.LocalIdent v,
    stkFp :: v
  }

mkStackFrame :: (E.ValueRepr v) => QBE.FuncDef -> v -> StackFrame v
mkStackFrame func = StackFrame func Map.empty

storeLocal :: QBE.LocalIdent -> v -> StackFrame v -> StackFrame v
storeLocal ident value frame@(StackFrame {stkVars = v}) =
  frame {stkVars = Map.insert ident value v}

lookupLocal :: StackFrame v -> QBE.LocalIdent -> Maybe v
lookupLocal (StackFrame {stkVars = v}) = flip Map.lookup v
{-# INLINEABLE lookupLocal #-}

------------------------------------------------------------------------

data SomeFunc m v
  = SSimFunc ([v] -> m (Maybe v))
  | SFuncDef QBE.FuncDef

-- This is an “abstract monad” representing the Simulator and allowing
-- interaction with an encapsulated Simulator state 'm'. The module
-- 'Language.QBE.Simulator.Default.State' provides an implementation of
-- a polymorphic Simulator state implement over a State monad.
--
-- The idea is inspired by Bourgeat et al. https://doi.org/10.1145/3607833
class (E.ValueRepr v, MonadThrow m) => Simulator m v | m -> v where
  isTrue :: v -> m Bool
  toAddress :: v -> m MEM.Address

  lookupSymbol :: QBE.GlobalIdent -> m (Maybe MEM.Address)
  findFunc :: QBE.GlobalIdent -> m (Maybe (SomeFunc m v))

  activeFrame :: m (StackFrame v)
  pushStackFrame :: StackFrame v -> m ()
  popStackFrame :: m (StackFrame v)
  getSP :: m v
  setSP :: v -> m ()

  writeMemory :: MEM.Address -> QBE.ExtType -> v -> m () -- TODO: LoadType?
  readMemory :: QBE.LoadType -> MEM.Address -> m v

liftMaybe :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
liftMaybe e Nothing = throwM e
liftMaybe _ (Just r) = pure r
{-# INLINE liftMaybe #-}

-- See https://c9x.me/compile/doc/il-v1.2.html#Subtyping
subType :: (Simulator m v) => QBE.BaseType -> v -> m v
subType baseTy v = liftMaybe TypingError $ subType' baseTy (E.getType v)
  where
    subType' QBE.Word (QBE.Base QBE.Word) = Just v
    subType' QBE.Word (QBE.Base QBE.Long) =
      E.extract (QBE.Base QBE.Word) v
    subType' QBE.Long (QBE.Base QBE.Long) = Just v
    subType' QBE.Single (QBE.Base QBE.Single) = Just v
    subType' QBE.Double (QBE.Base QBE.Double) = Just v
    subType' _ _ = Nothing
{-# INLINEABLE subType #-}

runBinary ::
  (Simulator m v) =>
  QBE.BaseType ->
  (v -> v -> Maybe v) ->
  v ->
  v ->
  m v
runBinary ty op lhs rhs =
  liftMaybe TypingError (op lhs rhs) >>= subType ty
{-# INLINEABLE runBinary #-}

modifyFrame :: (Simulator m v) => (StackFrame v -> StackFrame v) -> m ()
modifyFrame func = do
  frame <- popStackFrame
  pushStackFrame (func frame)
{-# INLINEABLE modifyFrame #-}

stackAlloc :: (Simulator m v) => v -> Word64 -> m v
stackAlloc size align = do
  stkPtr <- getSP
  let newStkPtr = stkPtr `E.sub` size >>= (`alignAddr` E.fromLit (QBE.Base QBE.Long) align)
  case newStkPtr of
    Just ptr -> setSP ptr >> pure ptr
    Nothing -> throwM InvalidAddressType
  where
    alignAddr addr alignment = addr `E.urem` alignment >>= (addr `E.sub`)
{-# INLINEABLE stackAlloc #-}

newStackFrame :: (Simulator m v) => QBE.FuncDef -> m (StackFrame v)
newStackFrame f = do
  frame <- getSP <&> mkStackFrame f
  pushStackFrame frame >> pure frame
{-# INLINEABLE newStackFrame #-}

returnFromFunc :: (Simulator m v) => m ()
returnFromFunc = popStackFrame >>= setSP . stkFp
{-# INLINE returnFromFunc #-}

maybeLookup :: (Simulator m v) => String -> Maybe a -> m a
maybeLookup name = liftMaybe (UnknownVariable name)
{-# INLINE maybeLookup #-}

lookupValue :: (Simulator m v) => QBE.BaseType -> QBE.Value -> m v
lookupValue ty (QBE.VConst (QBE.Const (QBE.Number v))) =
  pure $ E.fromLit (QBE.Base ty) v
lookupValue ty (QBE.VConst (QBE.Const (QBE.SFP v))) =
  subType ty (E.fromFloat v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.DFP v))) =
  subType ty (E.fromDouble v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.Global k))) = do
  v <- lookupSymbol k >>= maybeLookup (show k)
  subType ty (E.fromLit (QBE.Base QBE.Long) v)
lookupValue ty (QBE.VConst (QBE.Thread k)) = do
  v <- lookupSymbol k >>= maybeLookup (show k)
  subType ty (E.fromLit (QBE.Base QBE.Long) v)
lookupValue ty (QBE.VLocal k) = do
  v <- activeFrame >>= maybeLookup (show k) . flip lookupLocal k
  subType ty v
{-# INLINEABLE lookupValue #-}

lookupFunc :: (Simulator m v) => QBE.Value -> m (SomeFunc m v)
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

readNullArray :: (Simulator m v) => MEM.Address -> m [v]
readNullArray addr = go addr []
  where
    go a acc = do
      byte <- readMemory (QBE.LSubWord QBE.SignedByte) a
      if E.toWord64 byte == 0
        then pure acc
        else go (a + 1) (acc ++ [byte])
{-# INLINE readNullArray #-}
