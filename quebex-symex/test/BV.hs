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

          let ifT = SMT.bvLit 32 0xdeadbeef
          let ifF = SMT.bvLit 32 0xbeefdead
          let val = SMT.ite cond ifT ifF

          SMT.eq val ifT @?= cond
          SMT.eq val ifF @?= SMT.not cond,
      testCase "Folding of same-width extraction" $
        do
          let val = SMT.const "byte" 8
          SMT.extract val 0 8 @?= val,
      testCase "Folding of constant extractions" $
        do
          let val = SMT.bvLit 32 0xdeadbeef
          SMT.extract val 16 16 @?= SMT.bvLit 16 0xdead,
      testCase "Folding of identical nested extracts" $
        do
          let val = SMT.const "foobar" 64

          let ex1 = SMT.extract val 0 16
          let ex2 = SMT.extract ex1 0 16

          ex2 @?= ex1,
      testCase "Folding of ITE expressions" $
        do
          let cond = SMT.const "foo" 32 `SMT.eq` SMT.const "bar" 32
          let val = SMT.ite cond (SMT.bvLit 32 0xdeadbeef) (SMT.bvLit 32 0xbeefdead)

          SMT.extract val 0 16 @?= SMT.ite cond (SMT.bvLit 16 0xbeef) (SMT.bvLit 16 0xdead),
      testCase "Extract reduces size" $
        do
          let val = SMT.bvLit 32 0xdeadbeef
          SMT.extract val 8 8 @?= SMT.bvLit 8 0xbe,
      testCase "Removal of non-extracted zero-extensions" $
        do
          let val = SMT.zeroExtend 24 $ SMT.bvLit 8 0xff
          SMT.extract val 0 8 @?= SMT.bvLit 8 0xff
          SMT.extract val 4 4 @?= SMT.bvLit 4 0xf,
      testCase "Extraction of zero bits" $
        do
          let val = SMT.zeroExtend 24 $ SMT.bvLit 8 0xff
          SMT.extract val 8 24 @?= SMT.bvLit 24 0x0
          SMT.extract val 0 8 @?= SMT.bvLit 8 0xff
          SMT.extract val 24 8 @?= SMT.bvLit 8 0x0,
      -- TODO: constant fold extractions of zeros.
      -- SMT.extract val 8 8 @?= SMT.bvLit 8 0x0
      testCase "Extraction including zero-extended bits" $
        do
          let lit = SMT.bvLit 8 0xab
          let val = SMT.zeroExtend 56 lit
          SMT.extract val 0 32 @?= SMT.zeroExtend 24 lit
          SMT.extract val 0 64 @?= val
    ]

bvTests :: TestTree
bvTests = testGroup "SimpleBV" [foldingTests]
