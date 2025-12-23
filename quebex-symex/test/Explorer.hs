-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Explorer (exploreTests) where

import Data.Bifunctor (second)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Language.QBE (Program)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator (parseAndFind)
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Explorer (defSolver, explore, newEngine)
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

branchPoints :: [(ST.Assign, T.ExecTrace)] -> [[Bool]]
branchPoints lst = sort $ map (\(_, t) -> map fst t) lst

findAssign :: [(ST.Assign, T.ExecTrace)] -> [Bool] -> Maybe ST.Assign
findAssign [] _ = Nothing
findAssign ((a, eTrace) : xs) toFind
  | map fst eTrace == toFind = Just a
  | otherwise = findAssign xs toFind

explore' :: Program -> QBE.FuncDef -> [(String, QBE.BaseType)] -> IO [(ST.Assign, T.ExecTrace)]
explore' prog entry params = do
  engine <- newEngine <$> defSolver
  defEnv <- mkEnv prog 0 128 Nothing

  explore engine defEnv entry $
    map (second QBE.Base) params

------------------------------------------------------------------------

exploreTests :: TestTree
exploreTests =
  testGroup
    "Tests for Symbolic Program Exploration"
    [ testCase "Explore' program with four execution paths" $
        do
          let qbe =
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

          (prog, funcDef) <- parseAndFind (QBE.GlobalIdent "branchOnInput") qbe
          eTraces <- explore' prog funcDef [("cond1", QBE.Word), ("cond2", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False, False], [False, True], [True, False], [True, True]],
      testCase "Unsatisfiable branches" $
        do
          let qbe =
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

          (prog, funcDef) <- parseAndFind (QBE.GlobalIdent "branchOnInput") qbe
          eTraces <- explore' prog funcDef [("cond1", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False, False], [True, True]],
      testCase "Branch with overflow arithmetics" $
        do
          let qbe =
                "function $branchArithmetics(w %input) {\n\
                \@start\n\
                \%cond =w add %input, 1\n\
                \jnz %input, @branch.1, @branch.2\n\
                \@branch.1\n\
                \ret\n\
                \@branch.2\n\
                \ret\n\
                \}"

          (prog, funcDef) <- parseAndFind (QBE.GlobalIdent "branchArithmetics") qbe
          eTraces <- explore' prog funcDef [("input", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False], [True]]

          let assign = fromJust $ findAssign eTraces [False]
          Map.lookup "input" assign @?= Just (DE.VWord 0),
      testCase "Store symbolic value and memory, load it and pass it to function" $
        do
          let qbe =
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

          (prog, funcDef) <- parseAndFind (QBE.GlobalIdent "entry") qbe
          eTraces <- explore' prog funcDef [("x", QBE.Word)]

          let branches = branchPoints eTraces
          branches @?= [[False, False], [True, True]],
      testCase "Branch on a specific concrete 64-bit value" $
        do
          let qbe =
                "function $f(l %input.0) {\n\
                \@start\n\
                \%input.1 =l sub %input.0, 42\n\
                \jnz %input.1, @not42, @is42\n\
                \@not42\n\
                \ret\n\
                \@is42\n\
                \ret\n\
                \}"

          (prog, funcDef) <- parseAndFind (QBE.GlobalIdent "f") qbe
          eTraces <- explore' prog funcDef [("y", QBE.Long)]

          branchPoints eTraces @?= [[False], [True]]
          let assign = fromJust $ findAssign eTraces [False]
          Map.lookup "y" assign @?= Just (DE.VLong 42),
      testCase "Branching with subtyping" $
        do
          let qbe =
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

          (prog, funcDef) <- parseAndFind (QBE.GlobalIdent "f") qbe
          eTraces <- explore' prog funcDef [("y", QBE.Long), ("x", QBE.Word)]

          branchPoints eTraces @?= [[False], [True, False], [True, True]],
      testCase "make a single word symbolic" $
        do
          let qbe =
                "data $name = align 1 {  b \"abcd\", b 0 }\n\
                \function w $main() {\n\
                \@start\n\
                \%ptr =l alloc4 4\n\
                \call $quebex_symbolic_array(l %ptr, l 1, l 4, l $name)\n\
                \%word =w loadw %ptr\n\
                \jnz %word, @b1, @b2\n\
                \@b1\n\
                \ret 0\n\
                \@b2\n\
                \ret 1\n\
                \}"

          (prog, funcDef) <- parseAndFind (QBE.GlobalIdent "main") qbe
          eTraces <- explore' prog funcDef []

          length eTraces @?= 2,
      testCase "make a range of memory symbolic" $
        do
          let qbe =
                "data $name = align 1 {  b \"array\", b 0 }\n\
                \function w $main() {\n\
                \@start\n\
                \%ptr =l alloc4 32\n\
                \call $quebex_symbolic_array(l %ptr, l 8, l 4, l $name)\n\
                \%word =w loadw %ptr\n\
                \jnz %word, @b1, @b2\n\
                \@b1\n\
                \%ptr =l add %ptr, 4\n\
                \%word =w loadw %ptr\n\
                \jnz %word, @b3, @b4\n\
                \@b2\n\
                \ret 1\n\
                \@b3\n\
                \ret 0\n\
                \@b4\n\
                \ret 1\n\
                \}"

          (prog, funcDef) <- parseAndFind (QBE.GlobalIdent "main") qbe
          eTraces <- explore' prog funcDef []

          length eTraces @?= 3
    ]
