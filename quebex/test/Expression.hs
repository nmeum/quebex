-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Expression (exprTests) where

import Data.Int (Int64)
import Data.Maybe (fromJust)
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
          let lhs = E.fromLit (Q.Base Q.Word) 23 :: DE.RegVal
          let rhs = E.fromLit (Q.Base Q.Word) 42 :: DE.RegVal

          lhs `E.eq` lhs @?= truthValue
          lhs `E.ne` lhs @?= falseValue

          lhs `E.eq` rhs @?= falseValue
          lhs `E.ne` rhs @?= truthValue,
      testCase "Test unsigned comparison" $
        do
          let lhs = E.fromLit (Q.Base Q.Word) 23 :: DE.RegVal
          let rhs = E.fromLit (Q.Base Q.Word) 42 :: DE.RegVal

          lhs `E.ule` rhs @?= truthValue
          lhs `E.ule` lhs @?= truthValue
          lhs `E.ult` lhs @?= falseValue,
      testCase "Test signed comparision" $
        do
          let lhs = E.fromLit (Q.Base Q.Word) (fromIntegral (-1 :: Int64)) :: DE.RegVal
          let rhs = E.fromLit (Q.Base Q.Word) 0 :: DE.RegVal

          lhs `E.slt` rhs @?= truthValue
          lhs `E.sle` lhs @?= truthValue
          lhs `E.ult` rhs @?= falseValue,
      testCase "sar preserves sign bit" $
        do
          let v = E.fromLit (Q.Base Q.Word) (fromIntegral (-256 :: Int64)) :: DE.RegVal
          let r = fromJust $ v `E.sar` E.fromLit (Q.Base Q.Word) 1
          r @?= E.fromLit (Q.Base Q.Word) (fromIntegral (-128 :: Int64)),
      testCase "shr does not preserve sign bit" $
        do
          let v = E.fromLit (Q.Base Q.Word) (fromIntegral (-0x80000000 :: Int64)) :: DE.RegVal
          let r = fromJust $ v `E.shr` E.fromLit (Q.Base Q.Word) 8
          r @?= E.fromLit (Q.Base Q.Word) 0x800000,
      testCase "extend byte to word" $
        do
          let v = E.fromLit Q.Byte 128 :: DE.RegVal

          let signExt = E.fromLit (Q.Base Q.Word) 0xffffff80 :: DE.RegVal
          E.extend (Q.Base Q.Word) True v @?= Just signExt

          let zeroExt = E.fromLit (Q.Base Q.Word) 128 :: DE.RegVal
          E.extend (Q.Base Q.Word) False v @?= Just zeroExt,
      testCase "extend float" $
        do
          let s = E.fromLit (Q.Base Q.Single) 2342 :: DE.RegVal
          E.extend (Q.Base Q.Long) False s @?= Nothing

          let d = E.fromLit (Q.Base Q.Double) 2342 :: DE.RegVal
          E.extend (Q.Base Q.Long) False d @?= Nothing,
      testCase "current size exceeds extend" $
        do
          let s = E.fromLit (Q.Base Q.Word) 2342 :: DE.RegVal
          E.extend Q.Byte False s @?= Nothing,
      testCase "current size equals extend" $
        do
          let s = E.fromLit (Q.Base Q.Word) 2342 :: DE.RegVal
          E.extend (Q.Base Q.Word) True s @?= Nothing,
      testCase "extract from word" $
        do
          let v = E.fromLit (Q.Base Q.Word) 0xdeadbeef :: DE.RegVal

          let e1 = E.fromLit Q.Byte 0xef :: DE.RegVal
          E.extract Q.Byte v @?= Just e1

          let e2 = E.fromLit Q.HalfWord 0xbeef :: DE.RegVal
          E.extract Q.HalfWord v @?= Just e2,
      testCase "extract from float" $
        do
          let d = E.fromLit (Q.Base Q.Double) 2342 :: DE.RegVal
          E.extract Q.Byte d @?= Nothing
          E.extract (Q.Base Q.Single) d @?= Nothing

          let s = E.fromLit (Q.Base Q.Single) 2342 :: DE.RegVal
          E.extract Q.Byte s @?= Nothing
          E.extract (Q.Base Q.Single) s @?= Nothing,
      testCase "extract exceeds size" $
        do
          let v = E.fromLit (Q.Base Q.Word) 0xdeadbeef :: DE.RegVal
          E.extract (Q.Base Q.Long) v @?= Nothing
    ]
  where
    falseValue = Just (E.fromLit (Q.Base Q.Long) 0 :: DE.RegVal)
    truthValue = Just (E.fromLit (Q.Base Q.Long) 1 :: DE.RegVal)
