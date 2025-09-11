-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.List (find)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Explorer (defSolver, explore, logSolver, newEngine)
import Language.QBE.Types qualified as QBE
import System.Exit (ExitCode (ExitSuccess))
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile, withFile)
import Text.Parsec (ParseError)

-- OPTS:
--
--  -d dump queries
--  -D dump and unwind queries
--  -p print paths
--  -v print values for each path

data Opt
  = DumpQueries
  | DumpUnwind

newtype OptSet = OptSet [Opt]

hasFlag :: OptSet -> Opt -> Bool
hasFlag _ _ = True

------------------------------------------------------------------------

data ExecError
  = ParserError ParseError
  | UnknownFunction QBE.GlobalIdent
  deriving (Show)

instance Exception ExecError

exploreFile :: OptSet -> FilePath -> [(String, QBE.BaseType)] -> IO [(ST.Assign, T.ExecTrace)]
exploreFile opts filePath params = do
  content <- readFile filePath
  prog <- case parse filePath content of
    Right rt -> pure rt
    Left err -> throwIO err

  let funcs = globalFuncs prog
  func <- case find (\f -> QBE.fName f == entryFunc) funcs of
    Just x -> pure x
    Nothing -> throwIO $ UnknownFunction entryFunc

  if hasFlag opts DumpQueries
    then withFile "foo.smt2" WriteMode (exploreWithHandle prog func)
    else do
      engine <- defSolver >>= newEngine
      explore engine prog func params
  where
    entryFunc = QBE.GlobalIdent "main"

    exploreWithHandle prog func handle = do
      engine <- logSolver handle >>= newEngine
      explore engine prog func params

main :: IO ()
main = do
  putStrLn "hello!"
