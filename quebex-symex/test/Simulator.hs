module Simulator (simTests) where

import Control.Monad.State (gets)
import Data.List (find)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.State (envTracer)
import Language.QBE.Simulator.Symbolic.Tracer qualified as ST
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit
import Util

-- TODO: Code duplication with quebex/test/Simulator.hs
parseAndExec :: QBE.GlobalIdent -> [CE.Concolic] -> String -> IO ST.ExecTrace
parseAndExec funcName params input = do
  prog <- case parse "" input of
    Left e -> fail $ "Unexpected parsing error: " ++ show e
    Right r -> pure r

  func <- case find (\f -> QBE.fName f == funcName) (globalFuncs prog) of
    Just x -> pure x
    Nothing -> fail $ "Unknown function: " ++ show funcName

  sTrace <- runExec prog (execFunc func params >> gets envTracer) ST.newExecTrace
  case sTrace of
    Left e -> fail $ "Unexpected evaluation error: " ++ show e
    Right r -> pure r

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
      testCase "Branch tracing with symbolic branch" $
        do
          s <- getSolver
          c <- CE.unconstrained s "input" QBE.Word
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

          length t @?= 1
    ]

simTests :: TestTree
simTests =
  testGroup
    "Tests for the Symbolic Simulator"
    [traceTests]
