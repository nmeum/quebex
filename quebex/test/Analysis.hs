module Analysis (analTests) where

import Data.List (find)
import Data.Map qualified as Map
import Language.QBE (globalFuncs, parse)
import Language.QBE.Analysis.CFG qualified as CFG
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

-- TODO: Code duplication with parseAndExec from Simulator.hs
getFunction :: QBE.GlobalIdent -> String -> IO QBE.FuncDef
getFunction funcName input = do
  prog <- case parse "" input of
    Left e -> fail $ "Unexpected parsing error: " ++ show e
    Right r -> pure r

  case find (\f -> QBE.fName f == funcName) (globalFuncs prog) of
    Just x -> pure x
    Nothing -> fail $ "Unknown function: " ++ show funcName

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
          Map.lookup (QBE.BlockIdent "start") cfg @?= Just [QBE.BlockIdent "next"]
    ]
