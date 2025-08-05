-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Explorer (exploreTests) where

import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit
import Util

branchPoints :: [(ST.Assign, T.ExecTrace)] -> [[Bool]]
branchPoints lst = sort $ map (\(_, t) -> map fst t) lst

findAssign :: [(ST.Assign, T.ExecTrace)] -> [Bool] -> Maybe ST.Assign
findAssign [] _ = Nothing
findAssign ((a, eTrace) : xs) toFind
  | map fst eTrace == toFind = Just a
  | otherwise = findAssign xs toFind

------------------------------------------------------------------------

exploreTests :: TestTree
exploreTests =
  testGroup
    "Tests for Symbolic Program Exploration"
    [ testCase "Explore' program with four execution paths" $
        do
          prog <-
            parseProg
              "function $branchOnInput(w %cond1, w %cond2) {\n\
              \@jump.1\n\
              \jnz %cond1, @branch.1, @branch.2\n\
              \@branch.1\n\
              \jmp @jump.2\n\
              \@branch.2\n\
              \jmp @jump.2\n\
              \@jump.2\n\
              \jnz %cond2, @branch.3, @branch.4\n\
              \@branch.3\n\
              \ret\n\
              \@branch.4\n\
              \ret\n\
              \}"

          let funcDef = findFunc prog (QBE.GlobalIdent "branchOnInput")
          eTraces <- explore' prog funcDef [("cond1", QBE.Word), ("cond2", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False, False], [False, True], [True, False], [True, True]],
      testCase "Unsatisfiable branches" $
        do
          prog <-
            parseProg
              "function $branchOnInput(w %cond1) {\n\
              \@jump.1\n\
              \jnz %cond1, @branch.1, @branch.2\n\
              \@branch.1\n\
              \jmp @jump.2\n\
              \@branch.2\n\
              \jmp @jump.2\n\
              \@jump.2\n\
              \jnz %cond1, @branch.3, @branch.4\n\
              \@branch.3\n\
              \ret\n\
              \@branch.4\n\
              \ret\n\
              \}"

          let funcDef = findFunc prog (QBE.GlobalIdent "branchOnInput")
          eTraces <- explore' prog funcDef [("cond1", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False, False], [True, True]],
      testCase "Branch with overflow arithmetics" $
        do
          prog <-
            parseProg
              "function $branchArithmetics(w %input) {\n\
              \@start\n\
              \%cond =w add %input, 1\n\
              \jnz %input, @branch.1, @branch.2\n\
              \@branch.1\n\
              \ret\n\
              \@branch.2\n\
              \ret\n\
              \}"

          let funcDef = findFunc prog (QBE.GlobalIdent "branchArithmetics")
          eTraces <- explore' prog funcDef [("input", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False], [True]]

          let assign = fromJust $ findAssign eTraces [False]
          Map.lookup "input" assign @?= Just (DE.VWord 0),
      testCase "Store symbolic value and memory, load it and pass it to function" $
        do
          prog <-
            parseProg
              "function $branchOnInput(w %cond1) {\n\
              \@jump.1\n\
              \jnz %cond1, @branch.1, @branch.2\n\
              \@branch.1\n\
              \jmp @jump.2\n\
              \@branch.2\n\
              \jmp @jump.2\n\
              \@jump.2\n\
              \jnz %cond1, @branch.3, @branch.4\n\
              \@branch.3\n\
              \ret\n\
              \@branch.4\n\
              \ret\n\
              \}\n\
              \function $entry(w %in) {\n\
              \@start\n\
              \%a =l alloc4 4\n\
              \storew %in, %a\n\
              \%l =w loadw %a\n\
              \call $branchOnInput(w %l)\n\
              \ret\n\
              \}"

          let funcDef = findFunc prog (QBE.GlobalIdent "entry")
          eTraces <- explore' prog funcDef [("x", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False, False], [True, True]],
      testCase "Branch on a specific concrete 64-bit value" $
        do
          prog <-
            parseProg
              "function $f(l %input.0) {\n\
              \@start\n\
              \%input.1 =l sub %input.0, 42\n\
              \jnz %input.1, @not42, @is42\n\
              \@not42\n\
              \ret\n\
              \@is42\n\
              \ret\n\
              \}"

          let funcDef = findFunc prog (QBE.GlobalIdent "f")
          eTraces <- explore' prog funcDef [("y", QBE.Long)]

          branchPoints eTraces @?= [[False], [True]]
          let assign = fromJust $ findAssign eTraces [False]
          Map.lookup "y" assign @?= Just (DE.VLong 42),
      testCase "Branching with subtyping" $
        do
          prog <-
            parseProg
              "function $f(l %input.0, w %input.1) {\n\
              \@start\n\
              \%added =w add %input.1, 1\n\
              \%subed =w sub %added, %input.1\n\
              \%result =w add %added, %subed\n\
              \jnz %result, @b1, @b2\n\
              \@b1\n\
              \jnz %input.0, @b2, @b2\n\
              \@b2\n\
              \ret\n\
              \}"

          let funcDef = findFunc prog (QBE.GlobalIdent "f")
          eTraces <- explore' prog funcDef [("y", QBE.Long), ("x", QBE.Word)]

          branchPoints eTraces @?= [[False], [True, False], [True, True]]
    ]
