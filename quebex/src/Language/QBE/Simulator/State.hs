-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# LANGUAGE FunctionalDependencies #-}

-- | This module defines the abstract 'Simulator' monad and thus provides the primitives
-- used by "Language.QBE.Simulator" to describe the semantics of the QBE intermediate
-- representation.
module Language.QBE.Simulator.State
  ( -- * Abstract Monad
    Simulator (..),

    -- * Name Resolution
    SomeFunc (..),
    lookupFunc,
    lookupArgs,
    lookupGlobal,
    lookupLocal,
    lookupValue,

    -- * Helper
    liftMaybe,
    subType,
    runBinary,
    returnFromFunc,
    readNullArray,

    -- * Stack
    StackFrame (..),
    newStackFrame,
    storeLocal,
    modifyFrame,
    stackAlign,
    stackAlloc,
    stackSpill,
  )
where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Word (Word64)
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE

-- | Representation of a stack frame on the function call stack.
data StackFrame v
  = StackFrame
  { stkFunc :: QBE.FuncDef,
    stkVars :: Map.Map QBE.LocalIdent v,
    stkVarArgs :: [v],
    stkFp :: v
  }

-- | Create a new 'StackFrame' and push it onto the call stack.
newStackFrame ::
  (Simulator m v) =>
  -- | Definition of the functions to which this frame belongs.
  QBE.FuncDef ->
  -- | Named arguments passed to this function.
  Map.Map QBE.LocalIdent v ->
  -- | Optional, unnamed variadic arguments.
  [v] ->
  m (StackFrame v)
newStackFrame f args variadicArgs = do
  frame <- getSP <&> StackFrame f args variadicArgs
  pushStackFrame frame >> pure frame
{-# INLINEABLE newStackFrame #-}

-- | Store a local variable with a given name and value in the given 'StackFrame'.
storeLocal :: QBE.LocalIdent -> v -> StackFrame v -> StackFrame v
storeLocal ident value frame@(StackFrame {stkVars = v}) =
  frame {stkVars = Map.insert ident value v}

-- | Lookup a local variable in the current 'StackFrame'.
lookupLocal :: StackFrame v -> QBE.LocalIdent -> Maybe v
lookupLocal (StackFrame {stkVars = v}) = flip Map.lookup v
{-# INLINEABLE lookupLocal #-}

------------------------------------------------------------------------

-- | Representation of a function.
data SomeFunc m v
  = -- | A simulated function whose execution is intercepted by the Simulator.
    SSimFunc ([v] -> m (Maybe v))
  | -- | A QBE function defined in the input program.
    SFuncDef QBE.FuncDef

-- | This is an “abstract monad” representing the Simulator and allowing
-- interaction with an encapsulated Simulator state 'm'. Conceptually,
-- this monads describes the primitives based on which the semantics of
-- the QBE intermediate representation are abstractly described in
-- 'Language.QBE.Simulator'.
--
-- An instance of this monad then provides concrete semantics for these
-- primitives. For example, the module "Language.QBE.Simulator.Default.State"
-- provides an implementation of a polymorphic Simulator state implement over a
-- "Control.Monad.State" monad.
--
-- The idea is inspired by Bourgeat et al. <https://doi.org/10.1145/3607833>.
class (E.ValueRepr v, MonadError EvalError m) => Simulator m v | m -> v where
  -- | Check if a value of type 'ValueRepr' evaluates to true. This is used
  -- within "Language.QBE.Simulator" to implement conditional jumps.
  isTrue :: v -> m Bool

  -- | Convert a value of type 'ValueRepr' to a 'MEM.Address' that can be
  -- used to index a "Language.QBE.Simulator.Memory".
  toAddress :: v -> m MEM.Address

  -- | Lookup the address of a data symbol.
  lookupSymbol :: QBE.GlobalIdent -> m (Maybe MEM.Address)

  -- | Find a function by name, required to implement [call instructions](https://c9x.me/compile/doc/il-v1.2.html#Call).
  findFunc :: QBE.GlobalIdent -> m (Maybe (SomeFunc m v))

  -- | Find a function by "text segment" address, used for the implementation of function pointers.
  findFuncByAddr :: MEM.Address -> m (Maybe (SomeFunc m v))

  -- | Return the 'StackFrame' of the currently executed function.
  activeFrame :: m (StackFrame v)

  -- | Push a new 'StackFrame' onto the function call stack.
  pushStackFrame :: StackFrame v -> m ()

  -- | Pop the current stack frame from the function call stack.
  -- Should throw 'EmptyStack' when invoked on an empty function call stack.
  popStackFrame :: m (StackFrame v)

  -- | Get the current value of the stack pointer.
  getSP :: m v

  -- | Set the value of the stack pointer.
  setSP :: v -> m ()

  -- | Write a value to memory.
  writeMemory :: MEM.Address -> QBE.ExtType -> v -> m () -- TODO: LoadType?

  -- | Read a value from memory.
  readMemory :: QBE.LoadType -> MEM.Address -> m v

-- | Extracts the element out of a 'Just' or throw the given 'EvalError' if
-- if its argument is 'Nothing'.
liftMaybe :: (MonadError EvalError m) => EvalError -> Maybe a -> m a
liftMaybe e Nothing = throwError e
liftMaybe _ (Just r) = pure r
{-# INLINE liftMaybe #-}

-- | Implements the subtyping rules of the QBE intermediate representation.
--
-- See <https://c9x.me/compile/doc/il-v1.2.html#Subtyping>.
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

-- | Invoke a binary operation and perform subtyping (see 'subType') on its
-- results for the provided 'QBE.BaseType'. If the operation returns a 'Nothing'
-- value a 'TypingError' is raised.
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

-- | Modify the current 'StackFrame', e.g. to add a new local variable to it.
-- If the function call stack is currently empty an 'EmptyStack' error is thrown.
modifyFrame :: (Simulator m v) => (StackFrame v -> StackFrame v) -> m ()
modifyFrame func = do
  frame <- popStackFrame
  pushStackFrame (func frame)
{-# INLINEABLE modifyFrame #-}

-- | Align a stack address. Contrary to 'MEM.alignAddr', this rounds down to
-- the nearest aligned addressed (not up) as the stack grows downward. Further,
-- since the SP representation is presently not fixed, it operates on 'E.ValueRepr'.
stackAlign :: (E.ValueRepr v) => v -> v -> Maybe v
stackAlign addr alignment =
  addr `E.urem` alignment >>= (addr `E.sub`)
{-# INLINEABLE stackAlign #-}

-- | Allocate a given amount of bytes on the stack with the given alignment.
-- Advances the stack pointer accordingly.
stackAlloc :: (Simulator m v) => v -> Word64 -> m v
stackAlloc size align = do
  stkPtr <- getSP
  let newStkPtr = stkPtr `E.sub` size >>= (`stackAlign` E.fromLit (QBE.Base QBE.Long) align)
  case newStkPtr of
    Just ptr -> setSP ptr >> pure ptr
    Nothing -> throwError InvalidAddressType
{-# INLINEABLE stackAlloc #-}

-- | Allocate space for the given value on the stack and store it there.
-- Returns a reference (i.e., a memory address) fore the allocated memory.
stackSpill :: (Simulator m v) => v -> m MEM.Address
stackSpill val = do
  let ty = E.getType val
      size = fromIntegral $ QBE.extTypeByteSize ty
      sizeVal = E.fromLit (QBE.Base QBE.Long) size
  ptr <- stackAlloc sizeVal size >>= toAddress
  writeMemory ptr ty val
  pure ptr
{-# INLINEABLE stackSpill #-}

-- | Trigger a function return, popping its 'StackFrame' from the call stack
-- and updating both the stack and frame pointer.
returnFromFunc :: (Simulator m v) => m ()
returnFromFunc = popStackFrame >>= setSP . stkFp
{-# INLINE returnFromFunc #-}

maybeLookup :: (Simulator m v) => String -> Maybe a -> m a
maybeLookup name = liftMaybe (UnknownVariable name)
{-# INLINE maybeLookup #-}

-- | Lookup a global variable, might throw an 'UnknownVariable' error.
lookupGlobal :: (Simulator m v) => QBE.BaseType -> QBE.GlobalIdent -> m v
lookupGlobal ty name = do
  v <- lookupSymbol name >>= maybeLookup (show name)
  subType ty (E.fromLit (QBE.Base QBE.Long) v)
{-# INLINEABLE lookupGlobal #-}

-- | Lookup a 'QBE.Value', invoking the correct lookup function. For example,
-- 'lookupGlobal' for globals or 'lookupLocal' for local variables.
lookupValue :: (Simulator m v) => QBE.BaseType -> QBE.Value -> m v
lookupValue ty (QBE.VConst (QBE.Const (QBE.Number v))) =
  pure $ E.fromLit (QBE.Base ty) v
lookupValue ty (QBE.VConst (QBE.Const (QBE.SFP v))) =
  subType ty (E.fromFloat v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.DFP v))) =
  subType ty (E.fromDouble v)
lookupValue ty (QBE.VConst (QBE.Const (QBE.Global k))) = lookupGlobal ty k
lookupValue ty (QBE.VConst (QBE.Thread k)) = lookupGlobal ty k
lookupValue ty (QBE.VConst (QBE.Extern k)) = lookupGlobal ty k
lookupValue ty (QBE.VConst (QBE.ExternThread k)) = lookupGlobal ty k
lookupValue ty (QBE.VLocal k) = do
  v <- activeFrame >>= maybeLookup (show k) . flip lookupLocal k
  subType ty v
{-# INLINEABLE lookupValue #-}

lookupFuncName :: (Simulator m v) => QBE.GlobalIdent -> m (SomeFunc m v)
lookupFuncName name = do
  maybeFunc <- findFunc name
  case maybeFunc of
    Just def -> pure def
    Nothing -> throwError (UnknownFunction name)
{-# INLINEABLE lookupFuncName #-}

-- | Interpret the given 'QBE.Value' as a function reference, either
-- looking it up by name or by address. If the function could not be
-- found by address an 'UnknownFunctionAddr' is thrown, otherwise an
-- 'UnknownFunction' error is thrown.
lookupFunc :: (Simulator m v) => QBE.Value -> m (SomeFunc m v)
lookupFunc (QBE.VConst (QBE.Extern n)) = lookupFuncName n
lookupFunc (QBE.VConst (QBE.Const (QBE.Global n))) = lookupFuncName n
lookupFunc value = do
  addr <- lookupValue QBE.Long value >>= toAddress
  maybeFunc <- findFuncByAddr addr
  case maybeFunc of
    Just def -> pure def
    Nothing -> throwError (UnknownFunctionAddr addr)
{-# INLINEABLE lookupFunc #-}

lookupArg :: (Simulator m v) => QBE.FuncArg -> m (Maybe v)
lookupArg (QBE.ArgReg abity value) =
  Just <$> lookupValue (QBE.abityToBase abity) value
lookupArg (QBE.ArgEnv _) = error "env function parameters not supported"
lookupArg QBE.ArgVar = pure Nothing
{-# INLINEABLE lookupArg #-}

-- | Lookup the arguments to a function.
lookupArgs :: (Simulator m v) => [QBE.FuncArg] -> m [v]
lookupArgs args = catMaybes <$> mapM lookupArg args
{-# INLINE lookupArgs #-}

-- | Read a null-terminated C string from memory at the given 'MEM.Address'.
-- The return value is a list of 8-bit values.
readNullArray :: (Simulator m v) => MEM.Address -> m [v]
readNullArray addr = go addr []
  where
    go a acc = do
      byte <- readMemory (QBE.LSubWord QBE.SignedByte) a
      if E.toWord64 byte == 0
        then pure acc
        else go (a + 1) (acc ++ [byte])
{-# INLINE readNullArray #-}
