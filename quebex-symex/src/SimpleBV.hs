-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only
{-# LANGUAGE PatternSynonyms #-}

module SimpleBV
  ( SExpr,
    -- SMT.Solver,
    -- SMT.defaultConfig,
    -- SMT.newLogger,
    -- SMT.newLoggerWithHandle,
    -- SMT.newSolver,
    -- SMT.newSolverWithConfig,
    toSMT,
    not,
    eq,
    bvHex,
    bvAdd,
    bvAShr,
    concat,
    extract,
    const,
  )
where

-- import Control.Exception (assert)

import Data.Bits (shiftL, shiftR, (.&.))
import SimpleSMT qualified as SMT
import Prelude hiding (concat, const, not)

-- TODO

-- * Tests for expression eliminiation

-- * Cover the whole API

-- * view patterns

data Expr a
  = Var String
  | Int Integer
  | Not a
  | Eq a a
  | Add a a
  | AShr a a
  | Concat a a
  | Ite a a a
  | Extract Int Int a
  deriving (Show, Eq)

data SExpr
  = SExpr
  { width :: Int,
    sexpr :: Expr SExpr
  }
  deriving (Show, Eq)

-- TODO: view patterns
toSMT :: SExpr -> SMT.SExpr
toSMT (E (Var name)) = SMT.const name
toSMT SExpr {width = w, sexpr = Int v} = SMT.bvHex w v
toSMT (E (Not v)) = SMT.not (toSMT v)
toSMT (E (Eq lhs rhs)) = SMT.eq (toSMT lhs) (toSMT rhs)
toSMT (E (Concat lhs rhs)) = SMT.concat (toSMT lhs) (toSMT rhs)
toSMT (E (Extract _off _width _expr)) = error "unimplemented"

boolWidth :: Int
boolWidth = 1

pattern E :: Expr SExpr -> SExpr
pattern E expr <- SExpr {sexpr = expr, width = _}

const :: String -> Int -> SExpr
const name width = SExpr width (Var name)

-- declare :: SMT.Solver -> String -> SExpr -> IO SMT.SExpr
-- declare solver name = SMT.declare solver name . sexpr

bvHex :: Int -> Integer -> SExpr
bvHex width value = SExpr width (Int value)

-- ------------------------------------------------------------------------

not :: SExpr -> SExpr
not (E (Not cond)) = cond
not expr = expr {sexpr = Not expr}

eq' :: SExpr -> SExpr -> SExpr
eq' lhs rhs = SExpr boolWidth $ Eq lhs rhs

-- Eliminates ITE expressions when comparing with constants values, this is
-- useful in the QBE context to eliminate comparisons with truth values.
eq :: SExpr -> SExpr -> SExpr
eq lexpr@(E (Ite cond (E (Int ifT)) (E (Int ifF)))) rexpr@(E (Int other))
  | other == ifT = cond
  | other == ifF = not cond
  | otherwise = eq' lexpr rexpr
eq lhs rhs = eq' lhs rhs

concat' :: SExpr -> SExpr -> SExpr
concat' lhs rhs =
  SExpr (width lhs + width rhs) $ Concat lhs rhs

-- Replaces continuous concat expressions with a single extract expression.
concat :: SExpr -> SExpr -> SExpr
concat
  lhs@(E (Extract loff lwidth latom@(E (Var varLhs))))
  rhs@(E (Extract roff rwidth (E (Var varRhs))))
    | varLhs == varRhs && (roff + rwidth) == loff = extract' latom roff (lwidth + rwidth)
    | otherwise = concat' lhs rhs
concat lhs rhs = concat' lhs rhs

extract' :: SExpr -> Int -> Int -> SExpr
extract' expr off w = SExpr (width expr - w) $ Extract off w expr

-- Eliminate nested extract expression fo the same width.
extractNested :: SExpr -> Int -> Int -> SExpr
extractNested expr@(E (Extract ioff iwidth _)) off width =
  if ioff == off && iwidth == width
    then expr
    else extract' expr off width
extractNested expr off width = extract' expr off width

-- Performs direct extractions of constant immediate values.
extractConst :: SExpr -> Int -> Int -> SExpr
extractConst expr@(E (Int value)) off w =
  SExpr (width expr - w) . Int $ truncTo (value `shiftR` off) w
  where
    truncTo v bits = v .&. ((1 `shiftL` bits) - 1)
extractConst expr off width = extractNested expr off width

-- This performs constant propagation for subtyping of condition values (i.e.
-- the conversion from long to word).
extractIte :: SExpr -> Int -> Int -> SExpr
extractIte expr@(E (Ite cond ifT@(E (Int _)) ifF@(E (Int _)))) off w =
  let ex x = extractConst x off w
   in SExpr (width expr - w) $ Ite cond (ex ifT) (ex ifF)
extractIte expr off width = extractConst expr off width

extract :: SExpr -> Int -> Int -> SExpr
extract = extractIte

-- ------------------------------------------------------------------------

bvAdd :: SExpr -> SExpr -> SExpr
bvAdd x y = x {sexpr = Add x y}

bvAShr :: SExpr -> SExpr -> SExpr
bvAShr x y = x {sexpr = AShr x y}

-- bvAnd
-- bvBin
-- bvHex
-- bvLShr
-- bvMul
-- bvNeg
-- bvOr
-- bvSDiv
-- bvSLeq
-- bvSLt
-- bvSRem
-- bvShl
-- bvSub
-- bvUDiv
-- bvULeq
-- bvULt
-- bvURem
-- bvXOr
