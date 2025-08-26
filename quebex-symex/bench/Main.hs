-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Main (main) where

import Control.Monad (when)
import Criterion.Main
import Data.List (find)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Explorer (explore, logSolver, newEngine)
import Language.QBE.Types qualified as QBE
import SMTUnwind (unwind)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile, withFile)
import System.Process
  ( StdStream (CreatePipe, UseHandle),
    createProcess,
    proc,
    std_in,
    std_out,
    waitForProcess,
  )

logPath :: FilePath
logPath = "/tmp/quebex-symex-bench.smt2"

entryFunc :: QBE.GlobalIdent
entryFunc = QBE.GlobalIdent "entry"

------------------------------------------------------------------------

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

  withFile logPath WriteMode (explore' prog func)
  where
    explore' prog func handle = do
      engine <- logSolver handle >>= newEngine
      explore engine prog func params

getQueries :: String -> [(String, QBE.BaseType)] -> IO String
getQueries name params = do
  _ <- exploreQBE ("bench" </> "data" </> name) params
  -- XXX: Uncomment this to benchmark incremental solving instead.
  unwind logPath

solveQueries :: String -> IO ()
solveQueries queries = do
  devNull <- openFile "/dev/null" WriteMode
  (Just hin, _, _, p) <-
    createProcess
      (proc "bitwuzla" [])
        { std_in = CreatePipe,
          std_out = UseHandle devNull
        }

  hPutStrLn hin queries <* hClose hin
  ret <- waitForProcess p <* hClose devNull
  when (ret /= ExitSuccess) $
    error "SMT solver failed"

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "SMT Complexity"
        [ benchWithEnv "prime-numbers.qbe" [("a", QBE.Word)],
          benchWithEnv "bubble-sort.qbe" [("a", QBE.Word), ("b", QBE.Word), ("c", QBE.Word), ("d", QBE.Word)]
        ]
    ]
  where
    benchSolver :: String -> String -> Benchmark
    benchSolver name queries = bench name $ nfIO (solveQueries queries)

    benchWithEnv :: String -> [(String, QBE.BaseType)] -> Benchmark
    benchWithEnv name params = env (getQueries name params) (benchSolver name)
