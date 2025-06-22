{-# OPTIONS_GHC -Wno-x-partial #-}

module Symbolic (exprTests) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic (z3Solver)
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import Test.Tasty
import Test.Tasty.HUnit

------------------------------------------------------------------------

-- TODO: QuickCheck tests against the default interpreter's implementation.
storeTests :: TestTree
storeTests =
  testGroup
    "Storage Instance Tests"
    [ testCase "Create bitvector and convert it to bytes" $
        do
          s <- z3Solver
          let bytes = (E.toBytes (E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector) :: [SE.BitVector])
          values <- mapM (SMT.getExpr s . SE.getValue) bytes
          values @?= [SMT.Bits 8 0xef, SMT.Bits 8 0xbe, SMT.Bits 8 0xad, SMT.Bits 8 0xde],
      testCase "Convert bitvector to bytes and back" $
        do
          s <- z3Solver

          let bytes = (E.toBytes (E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector) :: [SE.BitVector])
          length bytes @?= 4

          value <- case E.fromBytes (QBE.Base QBE.Word) bytes of
            Just x -> SMT.getExpr s (SE.getValue x) <&> Just
            Nothing -> pure Nothing
          value @?= Just (SMT.Bits 32 0xdeadbeef)
    ]

valueReprTests :: TestTree
valueReprTests =
  testGroup
    "Symbolic ValueRepr Tests"
    [ testCase "Create from literal and add" $
        do
          s <- z3Solver

          let v1 = E.fromLit QBE.Word 127
          let v2 = E.fromLit QBE.Word 128

          expr <- SMT.getExpr s (SE.getValue $ fromJust $ v1 `E.add` v2)
          expr @?= SMT.Bits 32 0xff,
      testCase "Add incompatible values" $
        do
          let v1 = E.fromLit QBE.Word 0xffffffff :: SE.BitVector
          let v2 = E.fromLit QBE.Long 0xff :: SE.BitVector

          -- Note: E.add doesn't do subtyping if invoked directly
          (v1 `E.add` v2) @?= Nothing,
      testCase "Subtyping" $
        do
          s <- z3Solver

          let v1 = E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector
          let v2 = E.fromLit QBE.Long 0xff :: SE.BitVector

          let v1sub = fromJust $ E.subType QBE.Word v1
          v1sub' <- SMT.getExpr s (SE.getValue v1sub)
          v1sub' @?= SMT.Bits 32 0xdeadbeef

          let v2sub = fromJust $ E.subType QBE.Word v2
          v2sub' <- SMT.getExpr s (SE.getValue v2sub)
          v2sub' @?= SMT.Bits 32 0xff

          let subtypedAddExpr = v1sub `E.add` v2sub
          liftIO $ print subtypedAddExpr
          expr <- SMT.getExpr s (SE.getValue (fromJust subtypedAddExpr))

          expr @?= SMT.Bits 32 0xdeadbfee,
      testCase "Extend subwords" $
        do
          s <- z3Solver

          let bytes = (E.toBytes (E.fromLit QBE.Word 0xacacacac :: SE.BitVector) :: [SE.BitVector])
          let byte = head bytes

          let sext = fromJust $ E.extend QBE.SignedByte byte
          sextVal <- SMT.getExpr s (SE.getValue sext)
          sextVal @?= SMT.Bits 64 0xffffffffffffffac

          let zext = fromJust $ E.extend QBE.UnsignedByte byte
          zextVal <- SMT.getExpr s (SE.getValue zext)
          zextVal @?= SMT.Bits 64 0xac
    ]

exprTests :: TestTree
exprTests = testGroup "Expression tests" [storeTests, valueReprTests]
