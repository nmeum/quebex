-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Golden (goldenTests) where

import Data.Bifunctor (second)
import Data.List (find)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Explorer (defSolver, explore, newEngine)
import Language.QBE.Types qualified as QBE
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden.Advanced

type Result = Int

entryFunc :: QBE.GlobalIdent
entryFunc = QBE.GlobalIdent "entry"

exploreQBE :: FilePath -> [(String, QBE.BaseType)] -> IO Result
exploreQBE filePath params = do
  content <- readFile filePath
  prog <- case parse filePath content of
    Right rt -> pure rt
    Left err -> fail $ "Parsing error: " ++ show err

  let funcs = globalFuncs prog
  func <- case find (\f -> QBE.fName f == entryFunc) funcs of
    Just x -> pure x
    Nothing -> fail $ "Unable to find entry function: " ++ show entryFunc

  engine <- newEngine <$> defSolver
  defEnv <- mkEnv prog 0 128
  traces <-
    explore engine defEnv func $
      map (second QBE.Base) params
  pure $ length traces

simpleCmp :: Result -> Result -> IO (Maybe String)
simpleCmp expt act =
  return $
    if expt == act
      then Nothing
      else Just ("Exploration mismatch: " ++ err)
  where
    err :: String
    err = "expected=" ++ show expt ++ " actual=" ++ show act

runTest :: TestName -> Int -> [(String, QBE.BaseType)] -> TestTree
runTest testName expPaths params =
  goldenTest
    testName
    (pure expPaths)
    (exploreQBE fullPath params)
    simpleCmp
    (\_ -> pure ())
  where
    fullPath :: FilePath
    fullPath = "test" </> "golden" </> (testName ++ ".qbe")

------------------------------------------------------------------------

goldenTests :: TestTree
goldenTests =
  testGroup
    "goldenTests"
    [ runTest "three-branches" 3 [("a", QBE.Word), ("b", QBE.Word)],
      runTest "prime-numbers" 21 [("a", QBE.Word)],
      runTest "address-concretization" 2 [("a", QBE.Word)]
    ]
