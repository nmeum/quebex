module Main (main) where

import System.IO (withFile, IOMode(WriteMode))
import System.Process (shell, createProcess, waitForProcess)
import System.Exit (ExitCode(ExitSuccess))
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Data.List (find)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator.Explorer (explore, logSolver, newEngine)
import Language.QBE.Types qualified as QBE
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Backend.QuerySplitter (splitQueries)

logPath :: FilePath
logPath = "/tmp/quebex-symex-bench.smt2"

entryFunc :: QBE.GlobalIdent
entryFunc = QBE.GlobalIdent "entry"

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

getQueries :: FilePath -> FilePath -> [(String, QBE.BaseType)] -> IO ()
getQueries queryFp qbeFp params = do
  _ <- exploreQBE qbeFp params
  splitQueries logPath queryFp

solveQueries :: FilePath -> IO ()
solveQueries dirFp = do
  queries <- map (dirFp </>) <$> listDirectory dirFp
  forM_ queries solveQuery
 where
  solveQuery :: FilePath -> IO ()
  solveQuery fp = do
    -- TODO: use createProcess and do the input redirection in Haskell.
    (_, _, _, p) <-
       createProcess $ shell ("z3 -in -smt2 1>/dev/null 2>&1 <" ++ fp)

    ret <- waitForProcess p
    if ret /= ExitSuccess
      then error "SMT solver failed"
      else pure ()

main :: IO ()
main = do
  getQueries "/tmp/queries" "/home/soeren/src/quebex/quebex-symex/test/golden/prime-numbers.qbe" [("a", QBE.Word)]
  solveQueries "/tmp/queries"
