-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Main (main) where

import Control.Monad (when)
import Data.Char (isLower)
import Data.List (find, unsnoc)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Explorer (explore, logSolver, newEngine)
import Language.QBE.Types qualified as QBE
import Numeric (showFFloat)
import SMTUnwind (unwind)
import SimpleSMT qualified as SMT
import Statistics (mean, stddev)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hGetContents, hPutStrLn, withFile)
import System.Process
  ( StdStream (CreatePipe),
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

numRuns :: Int
numRuns = 3

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

getQueries :: FilePath -> [(String, QBE.BaseType)] -> IO String
getQueries qbeFp params = do
  _ <- exploreQBE qbeFp params
  unwind logPath

solveQueries :: String -> IO Double
solveQueries queries = do
  (Just hin, Just hout, _, p) <-
    createProcess
      (proc "z3" ["-smt2", "-st", "-in"])
        { std_in = CreatePipe,
          std_out = CreatePipe
        }
  hPutStrLn hin queries <* hClose hin

  ret <- waitForProcess p
  when (ret /= ExitSuccess) $
    error "SMT solver failed"

  -- transform the z3 output and extract the :total-time.
  outLines <- lines <$> hGetContents hout
  let stat = filter (not . all isLower) outLines
  exprs <- case SMT.readSExpr $ unlines stat of
    Just (SMT.List e, _) -> pure e
    _ -> error "invalid statistics output"
  timeStr <- case unsnoc exprs of
    Just (_, SMT.Atom s) -> pure s
    _ -> error "invalid statistics expression"
  pure $ read timeStr

runBench :: FilePath -> [(String, QBE.BaseType)] -> IO ()
runBench name params = do
  queries <- getQueries ("bench" </> "data" </> name) params
  totals <- mapM (\_ -> solveQueries queries) [1 .. numRuns]

  let meanStr = showFFloat (Just 3) (mean totals) "s"
  let devStr = showFFloat (Just 2) (stddev totals) "s"
  putStrLn $ name ++ "\t" ++ meanStr ++ " ± " ++ devStr

main :: IO ()
main = do
  runBench "bubble-sort.qbe" [("a", QBE.Word), ("b", QBE.Word), ("c", QBE.Word), ("d", QBE.Word)]
  runBench "prime-numbers.qbe" [("a", QBE.Word)]
