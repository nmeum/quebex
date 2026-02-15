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
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

getFunction :: QBE.GlobalIdent -> String -> IO QBE.FuncDef
getFunction funcName input = snd <$> parseAndFind funcName input

getFuncAndProg :: FilePath -> QBE.GlobalIdent -> IO QBE.FuncDef
getFuncAndProg fileName funcName =
  let filePath = "test" </> "testdata" </> fileName
   in readFile filePath >>= getFunction funcName

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

          -- “If Y is control dependent on X then X must have two exits.“, in
          -- this CFG there are no nodes with two exits: The CDG must be emtpy.
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
          cdg @?= [(QBE.BlockIdent "ifT", [QBE.BlockIdent "start"]), (QBE.BlockIdent "ifF", [QBE.BlockIdent "start"])],
      testCase "Compute CDG for code with loop" $
        do
          func <-
            getFunction
              (QBE.GlobalIdent "main")
              "function w $main() {\n\
              \@start\n\
              \%.1 =w copy 0\n\
              \%.2 =w copy 42\n\
              \%.3 =w copy 0\n\
              \@for_cond\n\
              \%.6 =w csltw %.3, %.2\n\
              \jnz %.6, @for_body, @for_join\n\
              \@for_body\n\
              \%.1 =w add %.1, 1\n\
              \@for_cont\n\
              \%.3 =w add %.3, 1\n\
              \jmp @for_cond\n\
              \@for_join\n\
              \ret %.11\n\
              \}\n"

          let cfg = CFG.build func
              toBlk = fromJust . CFG.labelToBasicBlock cfg

          let cdg = map (\(l, lst) -> (toBlk l, map toBlk $ S.toList lst)) $ M.toList (CDG.computeCDG cfg)
          cdg
            @?= [ (QBE.BlockIdent "=return", [QBE.BlockIdent "for_cond"]),
                  (QBE.BlockIdent "for_cond", [QBE.BlockIdent "for_cond"]),
                  (QBE.BlockIdent "for_body", [QBE.BlockIdent "for_cond"]),
                  (QBE.BlockIdent "for_cont", [QBE.BlockIdent "for_cond"])
                ],
      testCase "Compute CDG for code with two paths to node" $
        do
          func <- getFuncAndProg "disjunction.qbe" (QBE.GlobalIdent "main")

          let cfg = CFG.build func
              toBlk = fromJust . CFG.labelToBasicBlock cfg

          let cdg = map (\(l, lst) -> (toBlk l, map toBlk $ S.toList lst)) $ M.toList (CDG.computeCDG cfg)
          cdg
            @?= [ (QBE.BlockIdent "if_true.3", [QBE.BlockIdent "body.2"]),
                  (QBE.BlockIdent "if_true.5", [QBE.BlockIdent "if_true.3"]),
                  (QBE.BlockIdent "if_false.6", [QBE.BlockIdent "if_true.3"]),
                  (QBE.BlockIdent "if_join.7", [QBE.BlockIdent "if_true.3"]),
                  (QBE.BlockIdent "if_false.4", [QBE.BlockIdent "body.2", QBE.BlockIdent "if_true.3"])
                ]
    ]
