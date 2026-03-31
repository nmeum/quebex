-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module State (stateTests) where

import Control.Monad.State (runStateT)
import Data.Word (Word8)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Default.State (Env, loadObj, mkEnv)
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

stateTests :: TestTree
stateTests =
  testGroup
    "Test the default state implementation"
    [ testCase "loadObj returns end address" $
        do
          let obj = QBE.OItem QBE.Byte [QBE.DString "foobar"]

          env <- mkEnv [] 0x1000 128 :: IO (Env D.RegVal Word8)
          res <- fst <$> runStateT (loadObj 0x1000 obj) env

          res @?= 0x1006,
      testCase "loadObj accounts for member alignment" $
        do
          let o1 = QBE.OItem QBE.Byte [QBE.DString "foobar"]
          let o2 = QBE.OItem (QBE.Base QBE.Word) [QBE.DConst $ QBE.Number 23]

          env <- mkEnv [] 0x0 128 :: IO (Env D.RegVal Word8)
          res <- fst <$> runStateT (loadObj 0x0 o1 >>= flip loadObj o2) env

          -- o1: 0..6
          -- o2: 8..12
          res @?= 12

          -- The same alignment calculation should be performed by QBE.dataSize.
          let ds = QBE.DataDef [] (QBE.GlobalIdent "d") Nothing [o1, o2]
          12 @?= QBE.dataSize ds
    ]
