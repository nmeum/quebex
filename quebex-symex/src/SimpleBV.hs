-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only
{-# LANGUAGE PatternSynonyms #-}

module SimpleBV
  ( SExpr,
    SMT.Solver,
    SMT.defaultConfig,
    SMT.newLogger,
    SMT.newLoggerWithHandle,
    SMT.newSolver,
    SMT.newSolverWithConfig,
    SMT.setLogic,
    SMT.push,
    SMT.pop,
    SMT.popMany,
    SMT.check,
    SMT.Result (..),
    toSMT,
    ite,
    not,
    eq,
    bvHex,
    bvAdd,
    bvAShr,
    bvLShr,
    bvAnd,
    bvMul,
    bvNeg,
    bvOr,
    bvSDiv,
    bvSLeq,
    bvSLt,
    bvSRem,
    bvShl,
    bvSub,
    bvUDiv,
    bvULeq,
    bvULt,
    bvURem,
    bvXOr,
    concat,
    extract,
    const,
  )
where

-- import Control.Exception (assert)

import Control.Exception (assert)
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
  | Neg a
  | Not a
  | Eq a a
  | Add a a
  | AShr a a
  | LShr a a
  | And a a
  | Mul a a
  | Or a a
  | SDiv a a
  | SLeq a a
  | SLt a a
  | SRem a a
  | Shl a a
  | Sub a a
  | UDiv a a
  | ULeq a a
  | ULt a a
  | URem a a
  | XOr a a
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
toSMT (E (Neg v)) = SMT.bvNeg (toSMT v)
toSMT (E (Eq lhs rhs)) = SMT.eq (toSMT lhs) (toSMT rhs)
toSMT (E (Concat lhs rhs)) = SMT.concat (toSMT lhs) (toSMT rhs)
toSMT (E (Extract _off _width _expr)) = error "unimplemented"
toSMT (E (Mul lhs rhs)) = SMT.bvMul (toSMT lhs) (toSMT rhs)
toSMT (E (Or lhs rhs)) = SMT.bvOr (toSMT lhs) (toSMT rhs)
toSMT (E (SDiv lhs rhs)) = SMT.bvSDiv (toSMT lhs) (toSMT rhs)
toSMT (E (SLeq lhs rhs)) = SMT.bvSLeq (toSMT lhs) (toSMT rhs)
toSMT (E (SLt lhs rhs)) = SMT.bvSLt (toSMT lhs) (toSMT rhs)
toSMT (E (SRem lhs rhs)) = SMT.bvSRem (toSMT lhs) (toSMT rhs)
toSMT (E (Shl lhs rhs)) = SMT.bvShl (toSMT lhs) (toSMT rhs)
toSMT (E (Sub lhs rhs)) = SMT.bvSub (toSMT lhs) (toSMT rhs)
toSMT (E (UDiv lhs rhs)) = SMT.bvUDiv (toSMT lhs) (toSMT rhs)
toSMT (E (ULeq lhs rhs)) = SMT.bvULeq (toSMT lhs) (toSMT rhs)
toSMT (E (ULt lhs rhs)) = SMT.bvULt (toSMT lhs) (toSMT rhs)
toSMT (E (URem lhs rhs)) = SMT.bvURem (toSMT lhs) (toSMT rhs)
toSMT (E (XOr lhs rhs)) = SMT.bvXOr (toSMT lhs) (toSMT rhs)

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

---------------------------------------------------------------------------

ite :: SExpr -> SExpr -> SExpr -> SExpr
ite cond ifT ifF =
  assert (width ifT == width ifF) $
    SExpr (width ifT) (Ite cond ifT ifF)

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
extract' expr off w = SExpr w $ Extract off w expr

-- Eliminate extract expression where the value already has the desired bits.
extractSameWidth :: SExpr -> Int -> Int -> SExpr
extractSameWidth expr off w
  | off == 0 && width expr == w = expr
  | otherwise = extract' expr off w

-- Eliminate nested extract expression of the same width.
extractNested :: SExpr -> Int -> Int -> SExpr
extractNested expr@(E (Extract ioff iwidth _)) off width =
  if ioff == off && iwidth == width
    then expr
    else extractSameWidth expr off width
extractNested expr off width = extractSameWidth expr off width

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
extractIte (E (Ite cond ifT@(E (Int _)) ifF@(E (Int _)))) off w =
  let ex x = extractConst x off w
   in SExpr w $ Ite cond (ex ifT) (ex ifF)
extractIte expr off width = extractConst expr off width

extract :: SExpr -> Int -> Int -> SExpr
extract = extractIte

---------------------------------------------------------------------------

bvNeg :: SExpr -> SExpr
bvNeg x = x {sexpr = Neg x}

-- TODO: template-haskell

bvAdd :: SExpr -> SExpr -> SExpr
bvAdd x y = x {sexpr = Add x y}

bvAShr :: SExpr -> SExpr -> SExpr
bvAShr x y = x {sexpr = AShr x y}

bvLShr :: SExpr -> SExpr -> SExpr
bvLShr x y = x {sexpr = LShr x y}

bvAnd :: SExpr -> SExpr -> SExpr
bvAnd x y = x {sexpr = And x y}

bvMul :: SExpr -> SExpr -> SExpr
bvMul x y = x {sexpr = Mul x y}

bvOr :: SExpr -> SExpr -> SExpr
bvOr x y = x {sexpr = Or x y}

bvSDiv :: SExpr -> SExpr -> SExpr
bvSDiv x y = x {sexpr = SDiv x y}

bvSLeq :: SExpr -> SExpr -> SExpr
bvSLeq x y = x {sexpr = SLeq x y}

bvSLt :: SExpr -> SExpr -> SExpr
bvSLt x y = x {sexpr = SLt x y}

bvSRem :: SExpr -> SExpr -> SExpr
bvSRem x y = x {sexpr = SRem x y}

bvShl :: SExpr -> SExpr -> SExpr
bvShl x y = x {sexpr = Shl x y}

bvSub :: SExpr -> SExpr -> SExpr
bvSub x y = x {sexpr = Sub x y}

bvUDiv :: SExpr -> SExpr -> SExpr
bvUDiv x y = x {sexpr = UDiv x y}

bvULeq :: SExpr -> SExpr -> SExpr
bvULeq x y = x {sexpr = ULeq x y}

bvULt :: SExpr -> SExpr -> SExpr
bvULt x y = x {sexpr = ULt x y}

bvURem :: SExpr -> SExpr -> SExpr
bvURem x y = x {sexpr = URem x y}

bvXOr :: SExpr -> SExpr -> SExpr
bvXOr x y = x {sexpr = XOr x y}
