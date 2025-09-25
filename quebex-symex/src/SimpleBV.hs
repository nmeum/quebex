-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only
{-# LANGUAGE PatternSynonyms #-}

module SimpleBV
  -- ( SExpr,
  --   SMT.Solver,
  --   SMT.defaultConfig,
  --   SMT.newLogger,
  --   SMT.newLoggerWithHandle,
  --   SMT.newSolver,
  --   SMT.newSolverWithConfig,
  --   sexprToVal,
  --   declare,
  --   not,
  --   eq,
  --   bvHex,
  --   bvAdd,
  --   concat,
  -- )
where

-- import Control.Exception (assert)
import SimpleSMT qualified as SMT
import Prelude hiding (concat, not)

-- TODO https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms

-- data Value
--   = Value
--   { width :: Int,
--     atom  :: SMT.Atom
--   }

data Expr a
  = Var String
  | Int Integer
  | Not a
  | Eq a a
  | Add a a
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
toSMT SExpr { width = w, sexpr = Int v } = SMT.bvHex w v
toSMT (E (Not v)) = SMT.not (toSMT v)
toSMT (E (Eq lhs rhs)) = SMT.eq (toSMT lhs) (toSMT rhs)
toSMT (E (Concat lhs rhs)) = SMT.concat (toSMT lhs) (toSMT rhs)
toSMT (E (Extract _off _width _expr)) = error "unimplemented"

-- boolWidth :: Int
-- boolWidth = 1

pattern E :: Expr SExpr -> SExpr
pattern E expr <- SExpr {sexpr = expr, width = _}

const :: String -> Int -> SExpr
const name width = SExpr width (Var name)

--
-- declare :: SMT.Solver -> String -> SExpr -> IO SMT.SExpr
-- declare solver name = SMT.declare solver name . sexpr

bvHex :: Int -> Integer -> SExpr
bvHex width value = SExpr width (Int value)

-- ------------------------------------------------------------------------

not :: SExpr -> SExpr
not (E (Not cond)) = cond
not expr = expr {sexpr = Not (expr)}

eq' :: SExpr -> SExpr -> SExpr
eq' lhs rhs = lhs { sexpr = Eq lhs rhs }

-- Eliminates ITE expressions when comparing with constants values, this is
-- useful in the QBE context to eliminate comparisons with truth values.
eq :: SExpr -> SExpr -> SExpr
eq lexpr@(E (Ite cond (E (Int ifT)) (E (Int ifF)))) rexpr@(E (Int other)) =
  if other == ifT
    then cond
    else
      if other == ifF
        then not cond
        else eq' lexpr rexpr
eq lhs rhs = eq' lhs rhs

concat' :: SExpr -> SExpr -> SExpr
concat' lhs rhs =
  SExpr (width lhs + width rhs) $ Concat lhs rhs

-- Replaces continuous concat expressions with a single extract expression.
concat :: SExpr -> SExpr -> SExpr
concat
  lhs@(E (Extract loff lwidth latom@(E (Var varLhs))))
  rhs@(E (Extract roff rwidth (E (Var varRhs))))
    | varLhs == varRhs && (roff + rwidth) == loff = extract latom roff (lwidth + rwidth)
    | otherwise = concat' lhs rhs
concat lhs rhs = concat' lhs rhs

extract :: SExpr -> Int -> Int -> SExpr
extract expr off w = SExpr (width expr - w) $ Extract off w expr

-- TODO: use Int instead of Integer here.
-- extract :: SExpr -> Integer -> Integer -> SExpr
-- extract expr up 0
--   | width expr == (assert (up > 1) $ fromIntegral up - 1) = expr
--   | otherwise = error "todo"
--
-- ------------------------------------------------------------------------
--
bvAdd :: SExpr -> SExpr -> SExpr
bvAdd x y = x {sexpr = Add x y}
