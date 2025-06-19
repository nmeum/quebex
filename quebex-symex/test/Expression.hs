module Expression where

import Control.Monad (void)
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import SimpleSMT qualified as SMT
import Test.Tasty
import Test.Tasty.HUnit

getSolver :: IO (SMT.Solver)
getSolver = do
  s <- SMT.newSolver "z3" ["-in", "-smt2"] Nothing
  SMT.setLogic s "QF_BV"
  return s

------------------------------------------------------------------------

storeTests :: TestTree
storeTests =
  testGroup
    "Storage Tests"
    [ testCase "Create bitvector and convert it to bytes" $
        do
          s <- getSolver
          void $ SMT.check s

          let bytes = SE.toBytes $ SE.half 0x2342
          values <- mapM (\b -> SMT.getExpr s (SE.sexpr b)) bytes
          values @?= [SMT.Bits 8 0x42, SMT.Bits 8 0x23]
    ]

exprTests :: TestTree
exprTests = testGroup "Expression tests" [storeTests]
