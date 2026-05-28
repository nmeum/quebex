-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module State (stateTests) where

import Control.Monad.State (evalStateT)
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
          res <- evalStateT (loadObj 0x1000 obj) env

          res @?= 0x1006,
      -- TODO: Turn this into a QuickCheck 'testProperty'.
      testCase "loadObj return value is aligned with QBE.dataSize" $
        do
          let o1 = QBE.OItem QBE.Byte [QBE.DString "foobar"]
          let o2 = QBE.OItem (QBE.Base QBE.Word) [QBE.DConst $ QBE.Number 23]

          env <- mkEnv [] 0x0 128 :: IO (Env D.RegVal Word8)
          res <- evalStateT (loadObj 0x0 o1 >>= flip loadObj o2) env

          -- No padding inserted.
          res @?= 10

          -- The same calculation should be performed by QBE.dataSize.
          let ds = QBE.DataDef [] (QBE.GlobalIdent "d") Nothing [o1, o2]
          10 @?= QBE.dataSize ds
    ]
