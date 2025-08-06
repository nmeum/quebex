module Main (main) where

import Control.Monad (when)
import Data.Char (isLower)
import Data.List (find, unsnoc)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Backend.QuerySplitter (splitQueries)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Explorer (explore, logSolver, newEngine)
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import System.Directory (listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeBaseName, (</>))
import System.IO (IOMode (WriteMode), hGetContents, withFile)
import System.Process (StdStream (CreatePipe), createProcess, shell, std_out, waitForProcess)

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

getQueries :: FilePath -> FilePath -> [(String, QBE.BaseType)] -> IO FilePath
getQueries queryFp qbeFp params = do
  _ <- exploreQBE qbeFp params
  splitQueries logPath queryFp
  pure queryFp

solveQueries :: FilePath -> IO [(FilePath, Double)]
solveQueries dirFp = do
  queries <- map (dirFp </>) <$> listDirectory dirFp
  sltimes <- mapM solveQuery queries
  pure $ zip queries sltimes
  where
    solveQuery :: FilePath -> IO Double
    solveQuery fp = do
      -- TODO: use createProcess and do the input redirection in Haskell.
      (_, Just hout, _, p) <-
        createProcess $
          (shell ("z3 -st -in -smt2 2>/dev/null 2>&1 <" ++ fp))
            { std_out = CreatePipe
            }

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
runBench fp params = do
  results <- getQueries "/tmp/queries" fp params >>= solveQueries

  -- TODO: median, standard derivation, …
  let total = foldl (+) 0 $ map snd results
  putStrLn $ (takeBaseName fp) ++ "\t" ++ show total ++ "s"

main :: IO ()
main = do
  runBench "/home/soeren/src/quebex/quebex-symex/test/golden/prime-numbers.qbe" [("a", QBE.Word)]
