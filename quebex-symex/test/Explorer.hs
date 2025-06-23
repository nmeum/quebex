-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Explorer (exploreTests) where

import Language.QBE.Simulator.Explorer (explore)
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit
import Util

exploreTests :: TestTree
exploreTests =
  testGroup
    "Tests for Symbolic Program Exploration"
    [ testCase "Explore program with four execution paths" $
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
          eTraces <- explore prog funcDef [("cond1", QBE.Word), ("cond2", QBE.Word)]

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
          eTraces <- explore prog funcDef [("cond1", QBE.Word)]

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
          eTraces <- explore prog funcDef [("input", QBE.Word)]

          -- TODO: Obtain model for false Branch (input should be 0xffffffff).
          let branches = branchPoints eTraces
          branches @?= [[False], [True]],
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
          eTraces <- explore prog funcDef [("x", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False, False], [True, True]]
    ]
