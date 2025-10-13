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
    SMT.solverLogger,
    SMT.smtSolverLogger,
    SMT.setLogic,
    SMT.push,
    SMT.pop,
    SMT.popMany,
    SMT.check,
    SMT.Result (..),
    SMT.Value (..),
    pattern W,
    pattern Byte,
    pattern Half,
    pattern Word,
    pattern Long,
    width,
    const,
    declareBV,
    assert,
    sexprToVal,
    getValue,
    getValues,
    toSMT,
    ite,
    and,
    or,
    not,
    eq,
    bvLit,
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
    signExtend,
    zeroExtend,
  )
where

import Data.Bits (shiftL, shiftR, (.&.))
import SimpleSMT qualified as SMT
import Prelude hiding (and, concat, const, not, or)

data Expr a
  = Var String
  | Int Integer
  | And a a
  | Or a a
  | Neg a
  | Not a
  | Eq a a
  | BvAdd a a
  | BvAShr a a
  | BvLShr a a
  | BvAnd a a
  | BvMul a a
  | BvOr a a
  | BvSDiv a a
  | BvSLeq a a
  | BvSLt a a
  | BvSRem a a
  | BvShl a a
  | BvSub a a
  | BvUDiv a a
  | BvULeq a a
  | BvULt a a
  | BvURem a a
  | BvXOr a a
  | Concat a a
  | Ite a a a
  | Extract Int Int a
  | SignExtend Integer a
  | ZeroExtend Integer a
  deriving (Show, Eq)

data SExpr
  = SExpr
  { width :: Int,
    sexpr :: Expr SExpr
  }
  deriving (Show, Eq)

toSMT :: SExpr -> SMT.SExpr
toSMT expr =
  case sexpr expr of
    (Var name) -> SMT.const name
    (Int v) -> SMT.bvHex (width expr) v
    (Or lhs rhs) -> SMT.or (toSMT lhs) (toSMT rhs)
    (Ite cond lhs rhs) -> SMT.ite (toSMT cond) (toSMT lhs) (toSMT rhs)
    (And lhs rhs) -> SMT.and (toSMT lhs) (toSMT rhs)
    (Not v) -> SMT.not (toSMT v)
    (Neg v) -> SMT.bvNeg (toSMT v)
    (SignExtend n v) -> SMT.signExtend n (toSMT v)
    (ZeroExtend n v) -> SMT.zeroExtend n (toSMT v)
    (Eq lhs rhs) -> SMT.eq (toSMT lhs) (toSMT rhs)
    (Concat lhs rhs) -> SMT.concat (toSMT lhs) (toSMT rhs)
    (Extract o w e) -> SMT.extract (toSMT e) (fromIntegral $ o + w - 1) (fromIntegral o)
    (BvAnd lhs rhs) -> SMT.bvAnd (toSMT lhs) (toSMT rhs)
    (BvAShr lhs rhs) -> SMT.bvAShr (toSMT lhs) (toSMT rhs)
    (BvLShr lhs rhs) -> SMT.bvLShr (toSMT lhs) (toSMT rhs)
    (BvAdd lhs rhs) -> SMT.bvAdd (toSMT lhs) (toSMT rhs)
    (BvMul lhs rhs) -> SMT.bvMul (toSMT lhs) (toSMT rhs)
    (BvOr lhs rhs) -> SMT.bvOr (toSMT lhs) (toSMT rhs)
    (BvSDiv lhs rhs) -> SMT.bvSDiv (toSMT lhs) (toSMT rhs)
    (BvSLeq lhs rhs) -> SMT.bvSLeq (toSMT lhs) (toSMT rhs)
    (BvSLt lhs rhs) -> SMT.bvSLt (toSMT lhs) (toSMT rhs)
    (BvSRem lhs rhs) -> SMT.bvSRem (toSMT lhs) (toSMT rhs)
    (BvShl lhs rhs) -> SMT.bvShl (toSMT lhs) (toSMT rhs)
    (BvSub lhs rhs) -> SMT.bvSub (toSMT lhs) (toSMT rhs)
    (BvUDiv lhs rhs) -> SMT.bvUDiv (toSMT lhs) (toSMT rhs)
    (BvULeq lhs rhs) -> SMT.bvULeq (toSMT lhs) (toSMT rhs)
    (BvULt lhs rhs) -> SMT.bvULt (toSMT lhs) (toSMT rhs)
    (BvURem lhs rhs) -> SMT.bvURem (toSMT lhs) (toSMT rhs)
    (BvXOr lhs rhs) -> SMT.bvXOr (toSMT lhs) (toSMT rhs)

boolWidth :: Int
boolWidth = 1

pattern E :: Expr SExpr -> SExpr
pattern E expr <- SExpr {sexpr = expr, width = _}

pattern W :: Int -> SExpr
pattern W w <- SExpr {width = w}

pattern Byte :: SExpr
pattern Byte <- SExpr {width = 8}

pattern Half :: SExpr
pattern Half <- SExpr {width = 16}

pattern Word :: SExpr
pattern Word <- SExpr {width = 32}

pattern Long :: SExpr
pattern Long <- SExpr {width = 64}

------------------------------------------------------------------------

const :: String -> Int -> SExpr
const name width = SExpr width (Var name)

declareBV :: SMT.Solver -> String -> Int -> IO SExpr
declareBV solver name width = do
  let bits = SMT.tBits $ fromIntegral width
  SMT.declare solver name bits >> pure (const name width)

bvLit :: Int -> Integer -> SExpr
bvLit width value = SExpr width (Int value)

sexprToVal :: SExpr -> SMT.Value
sexprToVal (E (Var n)) = SMT.Other $ SMT.Atom n
sexprToVal e@(E (Int i)) = SMT.Bits (width e) i
sexprToVal _ = SMT.Other $ SMT.Atom "_"

assert :: SMT.Solver -> SExpr -> IO ()
assert solver = SMT.assert solver . toSMT

getValue :: SMT.Solver -> SExpr -> IO SMT.Value
getValue solver = SMT.getExpr solver . toSMT

getValues :: SMT.Solver -> [SExpr] -> IO [(String, SMT.Value)]
getValues solver exprs = do
  map go <$> SMT.getExprs solver (map toSMT exprs)
  where
    go :: (SMT.SExpr, SMT.Value) -> (String, SMT.Value)
    go (SMT.Atom name, value) = (name, value)
    go _ = error "non-atomic variable in inputVars"

---------------------------------------------------------------------------

ite :: SExpr -> SExpr -> SExpr -> SExpr
ite cond ifT ifF = SExpr (width ifT) (Ite cond ifT ifF)

not :: SExpr -> SExpr
not (E (Not cond)) = cond
not expr = expr {sexpr = Not expr}

and :: SExpr -> SExpr -> SExpr
and lhs rhs = lhs {sexpr = And lhs rhs}

or :: SExpr -> SExpr -> SExpr
or lhs rhs = lhs {sexpr = Or lhs rhs}

signExtend :: Integer -> SExpr -> SExpr
signExtend n expr = SExpr (width expr + fromIntegral n) $ SignExtend n expr

zeroExtend :: Integer -> SExpr -> SExpr
zeroExtend n expr = SExpr (width expr + fromIntegral n) $ ZeroExtend n expr

------------------------------------------------------------------------

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
  lhs@(E (Extract loff lwidth latom@(E exprLhs)))
  rhs@(E (Extract roff rwidth (E exprRhs)))
    | exprLhs == exprRhs && (roff + rwidth) == loff = extract latom roff (lwidth + rwidth)
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
extractConst (E (Int value)) off w =
  SExpr w . Int $ truncTo (value `shiftR` off) w
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

extractExt' ::
  (Integer -> SExpr -> Expr SExpr) ->
  SExpr ->
  Integer ->
  SExpr ->
  Int ->
  Int ->
  SExpr
extractExt' cons outer extBits inner exOff exWidth
  -- If we are only extracting the non-extended bytes...
  | width inner >= exOff + exWidth = extractIte inner exOff exWidth
  -- Consider: ((_ extract 31 0) ((_ zero_extend 56) byte))
  | exWidth < fromIntegral extBits && exOff == 0 =
      SExpr exWidth $ cons (extBits - fromIntegral exWidth) inner
  -- No folding...
  | otherwise = extractIte outer exOff exWidth

-- Remove ZeroExtend and SignExtend expression where we don't use
-- the extended bits because we extract below the extended size.
extractExt :: SExpr -> Int -> Int -> SExpr
extractExt expr@(E (SignExtend extBits inner)) exOff exWidth =
  extractExt' SignExtend expr extBits inner exOff exWidth
extractExt expr@(E (ZeroExtend extBits inner)) exOff exWidth =
  extractExt' ZeroExtend expr extBits inner exOff exWidth
extractExt expr off w = extractIte expr off w

extract :: SExpr -> Int -> Int -> SExpr
extract = extractExt

------------------------------------------------------------------------

binOp' :: (SExpr -> SExpr -> Expr SExpr) -> SExpr -> SExpr -> SExpr
binOp' op lhs rhs = lhs {sexpr = op lhs rhs}

binOp :: (SExpr -> SExpr -> Expr SExpr) -> SExpr -> SExpr -> SExpr
-- Consider: (bvslt ((_ zero_extend 24) byte0) ((_ zero_extend 24) byte1))
-- TODO: The following only works if 'op' does not consider sign-bits. Otherwise,
--       there is no semantic expression equivalence after this folding operation.
-- binOp op lhs@(E (ZeroExtend _ lhsInner)) rhs@(E (ZeroExtend _ rhsInner)) =
--   if width lhsInner == width rhsInner
--     then binOp op lhsInner rhsInner
--     else binOp' op lhs rhs
binOp op lhs rhs = binOp' op lhs rhs

-- TODO: Generate these using template-haskell.

bvNeg :: SExpr -> SExpr
bvNeg x = x {sexpr = Neg x}

bvAdd :: SExpr -> SExpr -> SExpr
bvAdd = binOp BvAdd

bvAShr :: SExpr -> SExpr -> SExpr
bvAShr = binOp BvAShr

bvLShr :: SExpr -> SExpr -> SExpr
bvLShr = binOp BvLShr

bvAnd :: SExpr -> SExpr -> SExpr
bvAnd = binOp BvAnd

bvMul :: SExpr -> SExpr -> SExpr
bvMul = binOp BvMul

bvOr :: SExpr -> SExpr -> SExpr
bvOr = binOp BvOr

bvSDiv :: SExpr -> SExpr -> SExpr
bvSDiv = binOp BvSDiv

bvSLeq :: SExpr -> SExpr -> SExpr
bvSLeq = binOp BvSLeq

bvSLt :: SExpr -> SExpr -> SExpr
bvSLt = binOp BvSLt

bvSRem :: SExpr -> SExpr -> SExpr
bvSRem = binOp BvSRem

bvShl :: SExpr -> SExpr -> SExpr
bvShl = binOp BvShl

bvSub :: SExpr -> SExpr -> SExpr
bvSub = binOp BvSub

bvUDiv :: SExpr -> SExpr -> SExpr
bvUDiv = binOp BvUDiv

bvULeq :: SExpr -> SExpr -> SExpr
bvULeq = binOp BvULeq

bvULt :: SExpr -> SExpr -> SExpr
bvULt = binOp BvULt

bvURem :: SExpr -> SExpr -> SExpr
-- Fold constant bvURem operations which are emitted a lot in our generated
-- SMT-LIB because of QBE's "shift-value modulo bitsize"-semantics.
bvURem v@(E (Int lhs)) (E (Int rhs)) =
  SExpr (width v) (Int $ lhs `rem` rhs)
bvURem lhs rhs = binOp BvURem lhs rhs

bvXOr :: SExpr -> SExpr -> SExpr
bvXOr = binOp BvXOr
