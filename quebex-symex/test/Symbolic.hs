-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# OPTIONS_GHC -Wno-x-partial #-}

module Symbolic (exprTests) where

import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Language.QBE.Simulator.Explorer (z3Solver)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import Test.Tasty
import Test.Tasty.HUnit

getSolver :: IO SMT.Solver
getSolver = do
  s <- z3Solver
  SMT.check s >> pure s

------------------------------------------------------------------------

-- TODO: QuickCheck tests against the default interpreter's implementation.
storeTests :: TestTree
storeTests =
  testGroup
    "Storage Instance Tests"
    [ testCase "Create bitvector and convert it to bytes" $
        do
          s <- getSolver
          let bytes = (E.toBytes (E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector) :: [SE.BitVector])
          values <- mapM (SMT.getExpr s . SE.toSExpr) bytes
          values @?= [SMT.Bits 8 0xef, SMT.Bits 8 0xbe, SMT.Bits 8 0xad, SMT.Bits 8 0xde],
      testCase "Convert bitvector to bytes and back" $
        do
          s <- getSolver

          let bytes = (E.toBytes (E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector) :: [SE.BitVector])
          length bytes @?= 4

          value <- case E.fromBytes (QBE.Base QBE.Word) bytes of
            Just x -> SMT.getExpr s (SE.toSExpr x) <&> Just
            Nothing -> pure Nothing
          value @?= Just (SMT.Bits 32 0xdeadbeef)
    ]

valueReprTests :: TestTree
valueReprTests =
  testGroup
    "Symbolic ValueRepr Tests"
    [ testCase "Create from literal and add" $
        do
          s <- getSolver

          let v1 = E.fromLit QBE.Word 127
          let v2 = E.fromLit QBE.Word 128

          expr <- SMT.getExpr s (SE.toSExpr $ fromJust $ v1 `E.add` v2)
          expr @?= SMT.Bits 32 0xff,
      testCase "Add incompatible values" $
        do
          let v1 = E.fromLit QBE.Word 0xffffffff :: SE.BitVector
          let v2 = E.fromLit QBE.Long 0xff :: SE.BitVector

          -- Note: E.add doesn't do subtyping if invoked directly
          (v1 `E.add` v2) @?= Nothing,
      testCase "Subtyping" $
        do
          s <- getSolver

          let v1 = E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector
          let v2 = E.fromLit QBE.Long 0xff :: SE.BitVector

          let v1sub = fromJust $ E.subType QBE.Word v1
          v1sub' <- SMT.getExpr s (SE.toSExpr v1sub)
          v1sub' @?= SMT.Bits 32 0xdeadbeef

          let v2sub = fromJust $ E.subType QBE.Word v2
          v2sub' <- SMT.getExpr s (SE.toSExpr v2sub)
          v2sub' @?= SMT.Bits 32 0xff

          let subtypedAddExpr = v1sub `E.add` v2sub
          expr <- SMT.getExpr s (SE.toSExpr (fromJust subtypedAddExpr))

          expr @?= SMT.Bits 32 0xdeadbfee,
      testCase "Extend subwords" $
        do
          s <- getSolver

          let bytes = (E.toBytes (E.fromLit QBE.Word 0xacacacac :: SE.BitVector) :: [SE.BitVector])
          let byte = head bytes

          let sext = fromJust $ E.extend QBE.SignedByte byte
          sextVal <- SMT.getExpr s (SE.toSExpr sext)
          sextVal @?= SMT.Bits 64 0xffffffffffffffac

          let zext = fromJust $ E.extend QBE.UnsignedByte byte
          zextVal <- SMT.getExpr s (SE.toSExpr zext)
          zextVal @?= SMT.Bits 64 0xac
    ]

exprTests :: TestTree
exprTests = testGroup "Expression tests" [storeTests, valueReprTests]
