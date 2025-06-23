-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Backend (backendTests) where

import Data.Maybe (fromJust)
import Language.QBE.Backend.DFS (findUnexplored, newPathSel, trackTrace)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Explorer (z3Solver)
import Language.QBE.Simulator.Symbolic.Tracer qualified as ST
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import Test.Tasty
import Test.Tasty.HUnit
import Util

traceTests :: TestTree
traceTests =
  testGroup
    "Tests for the Symbolic Tracer"
    [ testCase "Branch tracing with single concrete branch" $
        do
          t <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "function $main() {\n\
              \@start.1\n\
              \%cond =w add 0, 1\n\
              \jnz %cond, @branch.1, @branch.2\n\
              \@branch.1\n\
              \ret\n\
              \@branch.2\n\
              \ret\n\
              \}"

          -- Trace must be empty because it doesn't branch on symbolic values.
          length t @?= 0,
      testCase "Branch tracing and solving with single symbolic branch" $
        do
          s <- z3Solver
          c <- unconstrained s 0 "input" QBE.Word
          assertBool "created value is symbolic" $ CE.hasSymbolic c

          t <-
            parseAndExec
              (QBE.GlobalIdent "branchOnInput")
              [c]
              "function $branchOnInput(w %cond) {\n\
              \@start.1\n\
              \jnz %cond, @branch.1, @branch.2\n\
              \@branch.1\n\
              \ret\n\
              \@branch.2\n\
              \ret\n\
              \}"

          t @?= [(False, ST.newBranch (fromJust $ CE.symbolic c))]

          let pathSel = trackTrace newPathSel t
          (mm, nextPathSel) <- findUnexplored s pathSel
          case mm of
            (Just [(_, (SMT.Bits _ v))]) ->
              assertBool "condition must be /= 0" (v /= 0)
            _ -> assertFailure "unexpected model"

          -- There are only two branches: input == 0 and input /= 0
          (nxt, _) <- findUnexplored s nextPathSel
          nxt @?= Nothing,
      testCase "Tracing with multiple branches" $
        do
          s <- z3Solver
          c1 <- unconstrained s 0 "cond1" QBE.Word
          c2 <- unconstrained s 0 "cond2" QBE.Word

          t <-
            parseAndExec
              (QBE.GlobalIdent "branchOnInput")
              [c1, c2]
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

          length t @?= 2
    ]

backendTests :: TestTree
backendTests =
  testGroup
    "Tests for the Symbolic Data Structures"
    [traceTests]
