-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Expression where

import Data.Char (chr, ord)
import Data.Word (Word64)
import Language.QBE.Types qualified as QBE

-- | Generic expression abstraction operating on values of type 'QBE.ExtType'.
-- Values are either fixed-size bitvectors (8-, 16, 32-, or 64-bit) or
-- single-precision or double-precision floating point values.
class ValueRepr v where
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

  add :: v -> v -> Maybe v
  sub :: v -> v -> Maybe v
  mul :: v -> v -> Maybe v
  div :: v -> v -> Maybe v
  urem :: v -> v -> Maybe v
  srem :: v -> v -> Maybe v
  udiv :: v -> v -> Maybe v
  or :: v -> v -> Maybe v
  xor :: v -> v -> Maybe v
  and :: v -> v -> Maybe v

  neg :: v -> Maybe v

  sar :: v -> v -> Maybe v
  shr :: v -> v -> Maybe v
  shl :: v -> v -> Maybe v

  eq :: v -> v -> Maybe v
  ne :: v -> v -> Maybe v
  sle :: v -> v -> Maybe v
  slt :: v -> v -> Maybe v
  sge :: v -> v -> Maybe v
  sgt :: v -> v -> Maybe v
  ule :: v -> v -> Maybe v
  ult :: v -> v -> Maybe v
  uge :: v -> v -> Maybe v
  ugt :: v -> v -> Maybe v

fromString :: (ValueRepr v) => String -> [v]
fromString = map (\c -> fromLit QBE.Byte (fromIntegral $ ord c))

toString :: (ValueRepr v) => [v] -> String
toString = map (\b -> chr (fromIntegral $ toWord64 b))

boolToValue :: (ValueRepr v) => Bool -> v
boolToValue True = fromLit (QBE.Base QBE.Long) 1
boolToValue False = fromLit (QBE.Base QBE.Long) 0

compareExpr :: (ValueRepr v) => QBE.CmpOp -> (v -> v -> Maybe v)
compareExpr QBE.CEq = eq
compareExpr QBE.CNe = ne
compareExpr QBE.CSle = sle
compareExpr QBE.CSlt = slt
compareExpr QBE.CSge = sge
compareExpr QBE.CSgt = sgt
compareExpr QBE.CUle = ule
compareExpr QBE.CUlt = ult
compareExpr QBE.CUge = uge
compareExpr QBE.CUgt = ugt
{-# INLINE compareExpr #-}
