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

toBlkName :: CFG.CFG -> CFG.Label -> String
toBlkName cfg = show . fromJust . CFG.labelToBasicBlock cfg

cdgEdges :: CFG.CFG -> CDG.CDG -> [(String, [String])]
cdgEdges cfg cdg = map go (M.toList cdg)
  where
    go :: (CFG.Label, S.IntSet) -> (String, [String])
    go (l, lst) =
      (toBlkName cfg l, map (toBlkName cfg) (S.toList lst))

cfgEdges :: CFG.CFG -> [(String, String)]
cfgEdges cfg =
  let toBlk = toBlkName cfg
   in map (bimap toBlk toBlk) $ CFG.cfgEdges cfg

------------------------------------------------------------------------

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
          CFG.lookupSuccessors cfg (QBE.BlockIdent "start")
            @?= Just [QBE.BlockIdent "next"]

          cfgEdges cfg
            @?= [ ("@start", "@next"),
                  ("@next", "@=return")
                ]

          -- “If Y is control dependent on X then X must have two exits.“, in
          -- this CFG there are no nodes with two exits: The CDG must be emtpy.
          cdgEdges cfg (CDG.computeCDG cfg) @?= [],
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
              cdg = CDG.computeCDG cfg

          cdgEdges cfg cdg
            @?= [ ("@ifT", ["@start"]),
                  ("@ifF", ["@start"])
                ],
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
              cdg = CDG.computeCDG cfg

          cdgEdges cfg cdg
            @?= [ ("@=return", ["@for_cond"]),
                  ("@for_cond", ["@for_cond"]),
                  ("@for_body", ["@for_cond"]),
                  ("@for_cont", ["@for_cond"])
                ],
      testCase "Compute CDG for code with two paths to node" $
        do
          func <- getFuncAndProg "disjunction.qbe" (QBE.GlobalIdent "main")

          let cfg = CFG.build func
              cdg = CDG.computeCDG cfg

          cdgEdges cfg cdg
            @?= [ ("@if_true.3", ["@body.2"]),
                  ("@if_true.5", ["@if_true.3"]),
                  ("@if_false.6", ["@if_true.3"]),
                  ("@if_join.7", ["@if_true.3"]),
                  ("@if_false.4", ["@body.2", "@if_true.3"])
                ]
    ]
