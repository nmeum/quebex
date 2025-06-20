module Util where

import Control.Monad (void)
import SimpleSMT qualified as SMT

getSolver :: IO SMT.Solver
getSolver = do
  s <- SMT.newSolver "z3" ["-in", "-smt2"] Nothing
  SMT.setLogic s "QF_BV"
  void $ SMT.check s
  return s
