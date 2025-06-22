module Simulator (simTests) where

import Control.Monad.State (gets)
import Data.List (find, sort)
import Data.Maybe (fromJust)
import Language.QBE (Program, globalFuncs, parse)
import Language.QBE.Simulator
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.State (envTracer)
import Language.QBE.Simulator.Symbolic (explore, z3Solver)
import Language.QBE.Simulator.Symbolic.Tracer qualified as ST
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import Test.Tasty
import Test.Tasty.HUnit

parseProg :: String -> IO Program
parseProg input =
  case parse "" input of
    Left e -> fail $ "Unexpected parsing error: " ++ show e
    Right r -> pure r

findFunc :: Program -> QBE.GlobalIdent -> QBE.FuncDef
findFunc prog funcName =
  case find (\f -> QBE.fName f == funcName) (globalFuncs prog) of
    Just x -> x
    Nothing -> error $ "Unknown function: " ++ show funcName

-- TODO: Code duplication with quebex/test/Simulator.hs
parseAndExec :: QBE.GlobalIdent -> [CE.Concolic] -> String -> IO ST.ExecTrace
parseAndExec funcName params input = do
  prog <- parseProg input
  let func = findFunc prog funcName

  sTrace <- runExec prog (execFunc func params >> gets envTracer) ST.newExecTrace
  case sTrace of
    Left e -> fail $ "Unexpected evaluation error: " ++ show e
    Right r -> pure r

branchPoints :: [ST.ExecTrace] -> [[Bool]]
branchPoints = sort . map (map fst)

------------------------------------------------------------------------

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
          c <- CE.unconstrained s 0 "input" QBE.Word
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

          let pathSel = ST.trackTrace ST.newPathSel t
          (mm, newPathSel) <- ST.findUnexplored s pathSel
          case mm of
            (Just [(_, (SMT.Bits _ v))]) ->
              assertBool "condition must be /= 0" (v /= 0)
            _ -> assertFailure "unexpected model"

          -- There are only two branches: input == 0 and input /= 0
          (nxt, _) <- ST.findUnexplored s newPathSel
          nxt @?= Nothing,
      testCase "Tracing with multiple branches" $
        do
          s <- z3Solver
          c1 <- CE.unconstrained s 0 "cond1" QBE.Word
          c2 <- CE.unconstrained s 0 "cond2" QBE.Word

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
          branches @?= [[False, False], [True, True]]
    ]

simTests :: TestTree
simTests =
  testGroup
    "Tests for the Symbolic Simulator"
    [traceTests, exploreTests]
