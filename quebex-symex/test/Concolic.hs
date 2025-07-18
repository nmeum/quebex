-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Concolic (exprTests) where

import Data.Maybe (fromJust)
import Data.Word (Word8)
import Language.QBE.Simulator.Concolic.Expression qualified as C
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

-- TODO: QuickCheck tests against the default interpreter's implementation.
storeTests :: TestTree
storeTests =
  testGroup
    "Storage Instance Tests"
    -- TODO: Test case for partial concoilc bytes
    [ testCase "Convert concrete concolic value to bytes and back" $
        do
          let value = E.fromLit QBE.Word 0xdeadbeef :: C.Concolic D.RegVal

          let bytes = MEM.toBytes value :: [C.Concolic Word8]
          length bytes @?= 4

          let valueFromBytes = fromJust $ MEM.fromBytes (QBE.LBase QBE.Word) bytes
          C.concrete value @?= C.concrete valueFromBytes
    ]

exprTests :: TestTree
exprTests = testGroup "Expression tests" [storeTests]
