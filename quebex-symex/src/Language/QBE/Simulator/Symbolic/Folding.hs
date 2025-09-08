-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Symbolic.Folding
  ( notExpr,
    eqExpr,
    extractExpr,
    concatExpr,
  )
where

import Control.Exception (assert)
import Data.Bits (shiftL, shiftR, (.&.))
import SimpleSMT qualified as SMT

-- Performs the following transformation: (not (not X)) → X.
notExpr :: SMT.SExpr -> SMT.SExpr
notExpr (SMT.List [SMT.Atom "not", cond]) = cond
notExpr expr = SMT.not expr

-- Eliminates ITE expressions when comparing with constants values, this is
-- useful in the QBE context to eliminate comparisons with truth values.
eqExpr :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
eqExpr expr@(SMT.List [SMT.Atom "ite", cond@(SMT.List _), ifT, ifF]) o =
  case (SMT.sexprToVal ifT, SMT.sexprToVal ifF, SMT.sexprToVal o) of
    -- XXX: bit sizes must be equal, otherwise it would be invalid SMT-LIB.
    (SMT.Bits _ tv, SMT.Bits _ fv, SMT.Bits _ ov) ->
      if ov == tv
        then cond
        else
          if ov == fv
            then SMT.not cond
            else SMT.eq expr o
    _ -> SMT.eq expr o
eqExpr expr o = SMT.eq expr o

-- Replaces continuous concat expressions with a single extract expression.
concatExpr :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
concatExpr
  lhs@(SMT.List [SMT.List [SMT.Atom "_", SMT.Atom "extract", lx, ly], varLhs])
  rhs@(SMT.List [SMT.List [SMT.Atom "_", SMT.Atom "extract", rx, ry], varRhs])
    | varLhs == varRhs =
        case (SMT.sexprToVal lx, SMT.sexprToVal ly, SMT.sexprToVal rx, SMT.sexprToVal ry) of
          (SMT.Int lx', SMT.Int ly', SMT.Int rx', SMT.Int ry') ->
            if ly' == rx' + 1
              then extractExpr varLhs (fromIntegral ry') $ assert (lx' > ry') (fromIntegral $ lx' - ry' + 1)
              else SMT.concat lhs rhs
          _ -> error "unreachable" -- invalid SMT-LIB
    | otherwise = SMT.concat lhs rhs
concatExpr lhs rhs = SMT.concat lhs rhs

-- Alternative creation of `SMT.extract` expressions.
extract :: SMT.SExpr -> Int -> Int -> SMT.SExpr
extract expr off width =
  SMT.extract expr (fromIntegral $ off + width - 1) $ fromIntegral off

-- Eliminate nested extract expression fo the same width.
extractNested :: SMT.SExpr -> Int -> Int -> SMT.SExpr
extractNested
  expr@(SMT.List [SMT.List [SMT.Atom "_", SMT.Atom "extract", ix, iy], _])
  off
  width =
    case (SMT.sexprToVal ix, SMT.sexprToVal iy) of
      (SMT.Int ix', SMT.Int iy') ->
        if (fromIntegral ix' == off + width - 1) && fromIntegral iy' == off
          then expr
          else extract expr off width
      _ -> error "unreachable" -- invalid SMT-LIB
extractNested expr off width = extract expr off width

-- Performs direct extractions of constant immediate values.
extractConst :: SMT.SExpr -> Int -> Int -> SMT.SExpr
extractConst expr off width =
  case SMT.sexprToVal expr of
    SMT.Bits _ value ->
      SMT.bvBin width $ truncTo (value `shiftR` off) width
    _ -> extractNested expr off width
  where
    truncTo value bits = value .&. ((1 `shiftL` bits) - 1)

-- This performs constant propagation for subtyping of condition values (i.e.
-- the conversion from long to word).
extractITE :: SMT.SExpr -> Int -> Int -> SMT.SExpr
extractITE expr@(SMT.List [SMT.Atom "ite", cond@(SMT.List _), ifT, ifF]) off width =
  case (SMT.sexprToVal ifT, SMT.sexprToVal ifF) of
    (SMT.Bits _ _, SMT.Bits _ _) ->
      let ex x = extractConst x off width
       in SMT.List [SMT.Atom "ite", cond, ex ifT, ex ifF]
    _ -> extractNested expr off width -- not constant, skip extractConst
extractITE expr off width = extractConst expr off width

-- Chaining of multiple extract expression folding schemes.
extractExpr :: SMT.SExpr -> Int -> Int -> SMT.SExpr
extractExpr = extractITE
