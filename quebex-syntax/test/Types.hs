-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Types (typesTests) where

import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

typesTests :: TestTree
typesTests =
  testGroup
    "Utility functions provided by the Types module"
    [ testCase "objSize" $ do
        let o1 = QBE.OItem QBE.Byte [QBE.DString "foobar"]
            o2 = QBE.OItem (QBE.Base QBE.Word) [QBE.DConst $ QBE.Number 2342]

        let dataDef = QBE.DataDef [] (QBE.GlobalIdent "d") Nothing [o1, o2]
        10 @?= QBE.dataSize dataDef
    ]
