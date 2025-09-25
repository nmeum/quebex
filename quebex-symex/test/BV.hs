-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module BV (bvTests) where

import SimpleBV qualified as SMT
import Test.Tasty
import Test.Tasty.HUnit

foldingTests :: TestTree
foldingTests =
  testGroup
    "foldingTests"
    [ testCase "Folding of continous concat expressions" $
        do
          let val = SMT.const "foo" 32

          let b1 = SMT.extract val 0 8
          let b2 = SMT.extract val 8 8

          SMT.concat b2 b1 @?= SMT.extract val 0 16,
      testCase "Folding of ite-based equalities" $
        do
          let cond = SMT.const "foo" 32 `SMT.eq` SMT.const "bar" 32

          let ifT = SMT.bvBin 32 0xdeadbeef
          let ifF = SMT.bvBin 32 0xbeefdead
          let val = SMT.ite cond ifT ifF

          SMT.eq val ifT @?= cond
          SMT.eq val ifF @?= SMT.not cond,
      testCase "Folding of same-width extraction" $
        do
          let val = SMT.const "byte" 8
          SMT.extract val 0 8 @?= val,
      testCase "Folding of constant extractions" $
        do
          let val = SMT.bvBin 32 0xdeadbeef
          SMT.extract val 16 16 @?= SMT.bvBin 16 0xdead,
      testCase "Folding of identical nested extracts" $
        do
          let val = SMT.const "foobar" 64

          let ex1 = SMT.extract val 0 16
          let ex2 = SMT.extract ex1 0 16

          ex2 @?= ex1,
      testCase "Folding of ITE expressions" $
        do
          let cond = SMT.const "foo" 32 `SMT.eq` SMT.const "bar" 32
          let val = SMT.ite cond (SMT.bvBin 32 0xdeadbeef) (SMT.bvBin 32 0xbeefdead)

          SMT.extract val 0 16 @?= SMT.ite cond (SMT.bvBin 16 0xbeef) (SMT.bvBin 16 0xdead),
      testCase "Extract reduces size" $
        do
          let val = SMT.bvBin 32 0xdeadbeef
          SMT.extract val 8 8 @?= SMT.bvBin 8 0xbe
    ]

bvTests :: TestTree
bvTests = testGroup "SimpleBV" [foldingTests]
