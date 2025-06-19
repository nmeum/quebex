module Expression where

import Control.Monad (void)
import Data.Functor ((<&>))
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import Test.Tasty
import Test.Tasty.HUnit

getSolver :: IO SMT.Solver
getSolver = do
  s <- SMT.newSolver "z3" ["-in", "-smt2"] Nothing
  SMT.setLogic s "QF_BV"
  void $ SMT.check s
  return s

------------------------------------------------------------------------

storeTests :: TestTree
storeTests =
  testGroup
    "Storage Tests"
    [ testCase "Create bitvector and convert it to bytes" $
        do
          s <- getSolver
          let bytes = SE.toBytes $ SE.half 0x2342
          values <- mapM (SMT.getExpr s . SE.sexpr) bytes
          values @?= [SMT.Bits 8 0x42, SMT.Bits 8 0x23],
      testCase "Convert bitvector to bytes and back" $
        do
          s <- getSolver
          let bytes = SE.toBytes $ SE.half 0xbeef
          value <- case SE.fromBytes QBE.HalfWord bytes of
            Just x -> SMT.getExpr s (SE.sexpr x) <&> Just
            Nothing -> pure Nothing
          value @?= Just (SMT.Bits 16 0xbeef)
    ]

exprTests :: TestTree
exprTests = testGroup "Expression tests" [storeTests]
