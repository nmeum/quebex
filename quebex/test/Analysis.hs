-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Analysis (analTests) where

import Data.Bifunctor (bimap)
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.Maybe (fromJust)
import Language.QBE (parseAndFind)
import Language.QBE.Analysis.CDG qualified as CDG
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

          let toBlk = fromJust . CFG.labelToBasicBlock cfg
              edges = map (bimap toBlk toBlk) $ CFG.cfgEdges cfg
          edges @?= [(QBE.BlockIdent "start", QBE.BlockIdent "next"), (QBE.BlockIdent "next", QBE.BlockIdent "=return")]

          let cdg = map (\(l, lst) -> (toBlk l, map toBlk $ S.toList lst)) $ M.toList (CDG.computeCDG cfg)
          cdg @?= [],
      testCase "Generate CDG for code with single branch" $
        do
          func <-
            getFunction
              (QBE.GlobalIdent "foo")
              "function w $foo() {\n\
              \@start\n\
              \%val =w add 0, 1\n\
              \jnz %val, @ifT, @ifF\n\
              \@ifT\n\
              \ret 1\n\
              \@ifF\n\
              \ret 0\n\
              \}\n"

          let cfg = CFG.build func
              toBlk = fromJust . CFG.labelToBasicBlock cfg

          let cdg = map (\(l, lst) -> (toBlk l, map toBlk $ S.toList lst)) $ M.toList (CDG.computeCDG cfg)
          cdg @?= [(QBE.BlockIdent "ifT", [QBE.BlockIdent "start"]), (QBE.BlockIdent "ifF", [QBE.BlockIdent "start"])]
    ]
