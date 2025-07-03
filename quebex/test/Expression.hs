-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Expression (exprTests) where

import Data.Int (Int64)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Types qualified as Q
import Test.Tasty
import Test.Tasty.HUnit

exprTests :: TestTree
exprTests =
  testGroup
    "Expression Tests"
    [ testCase "Test equality" $
        do
          let lhs = E.fromLit Q.Word 23 :: DE.RegVal
          let rhs = E.fromLit Q.Word 42 :: DE.RegVal

          lhs `E.eq` lhs @?= truthValue
          lhs `E.ne` lhs @?= falseValue

          lhs `E.eq` rhs @?= falseValue
          lhs `E.ne` rhs @?= truthValue,
      testCase "Test unsigned comparison" $
        do
          let lhs = E.fromLit Q.Word 23 :: DE.RegVal
          let rhs = E.fromLit Q.Word 42 :: DE.RegVal

          lhs `E.ule` rhs @?= truthValue
          lhs `E.ule` lhs @?= truthValue
          lhs `E.ult` lhs @?= falseValue,
      testCase "Test signed comparision" $
        do
          let lhs = E.fromLit Q.Word (fromIntegral (-1 :: Int64)) :: DE.RegVal
          let rhs = E.fromLit Q.Word 0 :: DE.RegVal

          lhs `E.slt` rhs @?= truthValue
          lhs `E.sle` lhs @?= truthValue
          lhs `E.ult` rhs @?= falseValue
    ]
  where
    falseValue = Just (E.fromLit Q.Long 0 :: DE.RegVal)
    truthValue = Just (E.fromLit Q.Long 1 :: DE.RegVal)
