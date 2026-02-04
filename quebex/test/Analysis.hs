-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Analysis (analTests) where

import Language.QBE (parseAndFind)
import Language.QBE.Analysis.CFG qualified as CFG
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

getFunction :: QBE.GlobalIdent -> String -> IO QBE.FuncDef
getFunction funcName input = snd <$> parseAndFind funcName input

analTests :: TestTree
analTests =
  testGroup
    "Analysis tests"
    [ testCase "Simple CFG without any loops" $
        do
          func <-
            getFunction
              (QBE.GlobalIdent "foo")
              "function w $foo() {\n\
              \@start\n\
              \%val =w add 0, 1\n\
              \jmp @next\n\
              \@next\n\
              \ret\n\
              \}\n"

          let cfg = CFG.build func
          CFG.lookupSuccessors cfg (QBE.BlockIdent "start") @?= Just [QBE.BlockIdent "next"]
    ]
