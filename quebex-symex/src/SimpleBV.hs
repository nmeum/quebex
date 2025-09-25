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
-- import SimpleSMT qualified as SMT
-- import Prelude hiding (concat, not)

-- TODO https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms

-- data Value
--   = Value
--   { width :: Int,
--     atom  :: SMT.Atom
--   }

data Expr a
  = Var String
  | Not a
  | Eq a a
  | Concat a a
  | Extract Int Int a
  deriving (Show, Eq)

data SExpr
  = SExpr
  { width :: Int,
    sexpr :: Expr SExpr
  }
  deriving (Show, Eq)

-- boolWidth :: Int
-- boolWidth = 1

pattern E :: Expr SExpr -> SExpr
pattern E expr <- SExpr {sexpr = expr, width = _}

const :: String -> Int -> SExpr
const name width = SExpr width (Var name)

--
-- declare :: SMT.Solver -> String -> SExpr -> IO SMT.SExpr
-- declare solver name = SMT.declare solver name . sexpr
--
-- bvHex :: Int -> Integer -> SExpr
-- bvHex width value = SExpr width (SMT.bvHex width value)
--
-- sexprToVal :: SExpr -> SMT.Value
-- sexprToVal = SMT.sexprToVal . sexpr
--
-- ------------------------------------------------------------------------
--
-- pattern NotExpr :: SMT.SExpr -> SMT.SExpr
-- pattern NotExpr cond = SMT.List [SMT.Atom "not", cond]
--
not :: SExpr -> SExpr
not (E (Not cond)) = cond
not expr = expr {sexpr = Not (expr)}
--
-- pattern IteExpr :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
-- pattern IteExpr cond ifT ifF = SMT.List [SMT.Atom "ite", cond, ifT, ifF]
--
-- -- Eliminates ITE expressions when comparing with constants values, this is
-- -- useful in the QBE context to eliminate comparisons with truth values.
-- eq :: SExpr -> SExpr -> SExpr
-- eq (SMT expr@(IteExpr cond ifT ifF)) (SMT o) =
--   SExpr boolWidth $ case (SMT.sexprToVal ifT, SMT.sexprToVal ifF, SMT.sexprToVal o) of
--     -- XXX: bit sizes must be equal, otherwise it would be invalid SMT-LIB.
--     (SMT.Bits _ tv, SMT.Bits _ fv, SMT.Bits _ ov) ->
--       if ov == tv
--         then cond
--         else
--           if ov == fv
--             then SMT.not cond
--             else SMT.eq expr o
--     _ -> SMT.eq expr o
-- eq lhs rhs = lhs {sexpr = SMT.eq (sexpr lhs) (sexpr rhs)}
--
-- pattern ExtractExpr :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr -> SMT.SExpr
-- pattern ExtractExpr expr lowerBound upperBound =
--   SMT.List [SMT.List [SMT.Atom "_", SMT.Atom "extract", upperBound, lowerBound], expr]
--
concat' :: SExpr -> SExpr -> SExpr
concat' lhs rhs =
  SExpr (width lhs + width rhs) $ Concat lhs rhs

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
-- bvAdd :: SExpr -> SExpr -> SExpr
-- bvAdd x y =
--   assert (width x == width y) $
--     x {sexpr = SMT.bvAdd (sexpr x) (sexpr y)}
