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

          SMT.concat b2 b1 @?= SMT.extract val 0 16
    ]

bvTests :: TestTree
bvTests = testGroup "SimpleBV" [foldingTests]
