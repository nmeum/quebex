module Main (main) where

import Data.List (find)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator.Explorer (explore)
import Language.QBE.Types qualified as QBE
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden.Advanced

exploreQBE :: FilePath -> [(String, QBE.BaseType)] -> IO [(ST.Assign, T.ExecTrace)]
exploreQBE filePath params = do
  content <- readFile filePath
  prog <- case parse filePath content of
    Right rt -> pure rt
    Left err -> fail $ "Parsing error: " ++ show err

  let funcs = globalFuncs prog
  func <- case find (\f -> QBE.fName f == entryFunc) funcs of
    Just x -> pure x
    Nothing -> fail $ "Unable to find entry function: " ++ show entryFunc

  explore prog func params
