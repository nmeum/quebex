-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Expression where

import Data.Word (Word64)
import Language.QBE.Types qualified as QBE

class ValueRepr v where
  fromLit :: QBE.ExtType -> Word64 -> v
  fromFloat :: Float -> v
  fromDouble :: Double -> v
  toWord64 :: v -> Word64

  wordToLong :: QBE.SubLongType -> v -> Maybe v
  subType :: QBE.BaseType -> v -> Maybe v

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

  neg :: v -> v

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
