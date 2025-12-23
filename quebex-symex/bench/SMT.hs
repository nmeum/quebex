-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module SMT (smtBench) where

import Control.Monad (when)
import Criterion.Main
import Language.QBE (parseAndFind)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Concolic.State (mkEnv)
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
entryFunc = QBE.GlobalIdent "main"

------------------------------------------------------------------------

exploreQBE :: FilePath -> IO [(ST.Assign, T.ExecTrace)]
exploreQBE filePath = do
  (prog, func) <- readFile filePath >>= parseAndFind entryFunc

  withFile logPath WriteMode (explore' prog func)
  where
    explore' prog func handle = do
      engine <- newEngine <$> logSolver handle
      defEnv <- mkEnv prog 0 128 (Just 0)
      explore engine defEnv func []

getQueries :: String -> IO String
getQueries name = do
  _ <- exploreQBE ("bench" </> "data" </> "SMT" </> name)
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

smtBench :: Benchmark
smtBench = do
  bgroup
    "SMT Complexity"
    [ benchWithEnv "prime-numbers.qbe",
      benchWithEnv "bubble-sort.qbe",
      benchWithEnv "insertion-sort-uchar.qbe"
    ]
  where
    benchSolver :: String -> String -> Benchmark
    benchSolver name queries = bench name $ nfIO (solveQueries queries)

    benchWithEnv :: String -> Benchmark
    benchWithEnv name = env (getQueries name) (benchSolver name)
