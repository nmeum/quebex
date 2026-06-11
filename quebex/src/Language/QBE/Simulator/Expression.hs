-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

-- | This module provides a generic expression language used to describe
-- arithmetic and logic operations on instruction operands in the abstract
-- 'Language.QBE.Simulator' description of QBE semantics. Therefore, in
-- addition to the 'Language.QBE.Simulator.State.Simulator' monad, it is the
-- central component for the abstract description of QBE's semantics.
module Language.QBE.Simulator.Expression where

import Data.Char qualified as C
import Data.Word (Word64)
import Language.QBE.Types qualified as QBE

-- | Generic expression abstraction operating on values of type 'QBE.ExtType'.
-- Values are either fixed-size bitvectors (8-, 16, 32-, or 64-bit) or
-- single-precision or double-precision floating point values. The value type
-- must be tracked internally by the 'ValueRepr' instance. Operations on the
-- value must return 'Nothing' if the operation is performed on values of
-- different types.
class ValueRepr v where
  -- | Create a 'ValueRepr' from an integer literal.
  --
  -- TODO: rename fromLit to fromInt
  fromLit :: QBE.ExtType -> Word64 -> v

  fromFloat :: Float -> v
  fromDouble :: Double -> v
  toWord64 :: v -> Word64
  getType :: v -> QBE.ExtType

  floatToInt :: QBE.ExtType -> Bool -> v -> Maybe v
  intToFloat :: QBE.ExtType -> Bool -> v -> Maybe v
  extendFloat :: v -> Maybe v
  truncFloat :: v -> Maybe v

  -- | Extend a value to the given 'QBE.ExtType'. The 'Bool' is true if
  -- the value should be sign-extended, otherwise it is zero-extended.
  -- If the 'v' is a float or if the current size exceeds (or is equal to)
  -- the size of 'QBE.ExtType', then 'Nothing' is returned.
  extend :: QBE.ExtType -> Bool -> v -> Maybe v

  -- | Extract the least significant bits of a 'v'. The bits to extract
  -- are deduced from the given 'ExtType'. Returns 'Nothing' if the
  -- 'ExtType' is a float type, if the value is a float, or if the size
  -- of 'ExtType' exceeds the size of 'v'.
  extract :: QBE.ExtType -> v -> Maybe v

  -- | Addition.
  add :: v -> v -> Maybe v

  -- | Subtraction.
  sub :: v -> v -> Maybe v

  -- | Multiplication.
  mul :: v -> v -> Maybe v

  -- | Unsigned division.
  div :: v -> v -> Maybe v

  -- | Unsigned remainder.
  urem :: v -> v -> Maybe v

  -- | Signed remainder.
  srem :: v -> v -> Maybe v

  -- | Unsigned division.
  udiv :: v -> v -> Maybe v

  -- | Bitwise or.
  or :: v -> v -> Maybe v

  -- | Bitwise xor.
  xor :: v -> v -> Maybe v

  -- | Bitwise and.
  and :: v -> v -> Maybe v

  -- | Unary negation.
  neg :: v -> Maybe v

  -- | Arithmetic right shift, preserving the sign bit of the shifted value.
  -- Shift amount must always be a 32-bit value, the shifted value must be 32- or 64-bit.
  sar :: v -> v -> Maybe v

  -- | Logical shift right, filling the newly freed bits with zeroes.
  -- Shift amount must always be a 32-bit value, the shifted value must be 32- or 64-bit.
  shr :: v -> v -> Maybe v

  -- | Logical shift left, always fills the freed bits with zeroes.
  -- Shift amount must always be a 32-bit value, the shifted value must be 32- or 64-bit.
  shl :: v -> v -> Maybe v

  -- | Check for equality.
  eq :: v -> v -> Maybe v

  -- | Check if two values are not equal.
  ne :: v -> v -> Maybe v

  -- | Signed less than or equal to.
  sle :: v -> v -> Maybe v

  -- | Signed less than.
  slt :: v -> v -> Maybe v

  -- | Signed greater than or equal to.
  sge :: v -> v -> Maybe v

  -- | Signed greater than.
  sgt :: v -> v -> Maybe v

  -- | Unsigned less than or equal to.
  ule :: v -> v -> Maybe v

  -- | Unsigned less than.
  ult :: v -> v -> Maybe v

  -- | Unsigned greater than or equal to.
  uge :: v -> v -> Maybe v

  -- | Unsigned greater then.
  ugt :: v -> v -> Maybe v

  -- | Ordered, no operand is a NaN.
  -- Only defined for floating points, must return 'Nothing' otherwise.
  ord :: v -> v -> Maybe v

  -- | Unordered, at least one operand is a NaN.
  -- Only defined for floating points, must return 'Nothing' otherwise.
  unord :: v -> v -> Maybe v
  unord lhs rhs = ord lhs rhs >>= neg

-- | Convert a string to a list of 8-bit values represented through 'ValueRepr'.
fromString :: (ValueRepr v) => String -> [v]
fromString = map (\c -> fromLit QBE.Byte (fromIntegral $ C.ord c))

-- | Inverse of 'fromString'.
toString :: (ValueRepr v) => [v] -> String
toString = map (\b -> C.chr (fromIntegral $ toWord64 b))

-- | Convert a Boolean value to a 64-bit value in 'ValueRepr'.
boolToValue :: (ValueRepr v) => Bool -> v
boolToValue True = fromLit (QBE.Base QBE.Long) 1
boolToValue False = fromLit (QBE.Base QBE.Long) 0

-- | Map a 'QBE.IntCmpOp' to the corresponding function from 'ValueRepr'.
compareIntExpr :: (ValueRepr v) => QBE.IntCmpOp -> (v -> v -> Maybe v)
compareIntExpr QBE.IEq = eq
compareIntExpr QBE.INe = ne
compareIntExpr QBE.ISle = sle
compareIntExpr QBE.ISlt = slt
compareIntExpr QBE.ISge = sge
compareIntExpr QBE.ISgt = sgt
compareIntExpr QBE.IUle = ule
compareIntExpr QBE.IUlt = ult
compareIntExpr QBE.IUge = uge
compareIntExpr QBE.IUgt = ugt
{-# INLINE compareIntExpr #-}

-- | Map a 'QBE.FloatCmpOp' to the corresponding function from 'ValueRepr'.
compareFloatExpr :: (ValueRepr v) => QBE.FloatCmpOp -> (v -> v -> Maybe v)
compareFloatExpr QBE.FEq = eq
compareFloatExpr QBE.FNe = ne
compareFloatExpr QBE.FLe = sle
compareFloatExpr QBE.FLt = slt
compareFloatExpr QBE.FGe = sge
compareFloatExpr QBE.FGt = sgt
compareFloatExpr QBE.FOrd = ord
compareFloatExpr QBE.FUnord = unord
{-# INLINE compareFloatExpr #-}
