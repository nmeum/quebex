-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module SimpleBV
  ( SExpr,
    SMT.Solver,
    SMT.defaultConfig,
    SMT.newLogger,
    SMT.newLoggerWithHandle,
    SMT.newSolver,
    SMT.newSolverWithConfig,
    declare,
    not,
    bvHex,
    bvAdd,
  )
where

import Control.Exception (assert)
import SimpleSMT qualified as SMT
import Prelude hiding (not)

-- TODO https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms

data SExpr
  = SExpr
  { width :: Int,
    sexpr :: SMT.SExpr
  }

declare :: SMT.Solver -> String -> SExpr -> IO SMT.SExpr
declare solver name = SMT.declare solver name . sexpr

bvHex :: Int -> Integer -> SExpr
bvHex width value = SExpr width (SMT.bvHex width value)

------------------------------------------------------------------------

not :: SExpr -> SExpr
not v@(SExpr {sexpr = (SMT.List [SMT.Atom "not", cond])}) = v {sexpr = cond}
not expr = expr {sexpr = SMT.not (sexpr expr)}

------------------------------------------------------------------------

bvAdd :: SExpr -> SExpr -> SExpr
bvAdd x y =
  assert (width x == width y) $
    x {sexpr = SMT.bvAdd (sexpr x) (sexpr y)}
