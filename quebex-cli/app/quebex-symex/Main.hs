-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Data.Binary (encodeFile)
import Data.KTest (KTest (KTest), KTestObj, fromAssign)
import Data.String (fromString)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.CmdLine qualified as CMD
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Explorer (defSolver, explore, logSolver, newEngine)
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT
import System.Directory (createDirectoryIfMissing)
import System.FilePath (addExtension, (</>))
import System.IO (IOMode (WriteMode), withFile)
import Text.Printf (printf)

data Opts = Opts
  { optLog :: Maybe FilePath,
    optSeed :: Maybe Int,
    optTests :: Maybe FilePath,
    optBase :: CMD.BasicArgs
  }

optsParser :: OPT.Parser Opts
optsParser =
  Opts
    <$> OPT.optional
      ( OPT.strOption
          ( OPT.long "dump-smt2"
              <> OPT.short 'd'
              <> OPT.metavar "FILE"
              <> OPT.help "Output queries as an SMT-LIB file"
          )
      )
    <*> OPT.optional
      ( OPT.option
          OPT.auto
          ( OPT.long "random-seed"
              <> OPT.short 'r'
              <> OPT.help "Initial seed to for the random number generator"
          )
      )
    <*> OPT.optional
      ( OPT.strOption
          ( OPT.long "write-tests"
              <> OPT.short 't'
              <> OPT.metavar "DIR"
              <> OPT.help "Write .ktest files to the given directory"
          )
      )
    <*> CMD.basicArgs

------------------------------------------------------------------------

exploreFile :: Opts -> IO [(ST.Assign, T.ExecTrace)]
exploreFile opts@Opts {optBase = base} = do
  (prog, func) <- CMD.parseEntryFile $ CMD.optQBEFile base

  env <- mkEnv prog (CMD.optMemStart base) (CMD.optMemSize base) (optSeed opts)
  case optLog opts of
    Just fn -> withFile fn WriteMode (exploreWithHandle env func)
    Nothing -> do
      engine <- newEngine <$> defSolver
      explore engine env func params
  where
    params :: [(String, QBE.ExtType)]
    params = []

    exploreWithHandle env func handle = do
      engine <- newEngine <$> logSolver handle
      explore engine env func params

writeKTests :: FilePath -> FilePath -> [[KTestObj]] -> IO ()
writeKTests directory fileArg objs = do
  createDirectoryIfMissing True directory
  mapM_ (uncurry writeKTest) $ zip [1 ..] (map mkTestCase objs)
  where
    mkTestCase :: [KTestObj] -> KTest
    mkTestCase = KTest [fromString fileArg]

    writeKTest :: Int -> KTest -> IO ()
    writeKTest n ktest = do
      flip encodeFile ktest $
        addExtension
          (directory </> ("test" ++ printf "%06d" n))
          ".ktest"

main :: IO ()
main = do
  args <- OPT.execParser cmd
  paths <- exploreFile args

  putStrLn $ "\n---\nAmount of paths: " ++ show (length paths)
  case optTests args of
    Just dir ->
      writeKTests dir (CMD.optQBEFile $ optBase args) $
        map (fromAssign . fst) paths
    Nothing -> pure ()
  where
    cmd :: OPT.ParserInfo Opts
    cmd =
      OPT.info
        (optsParser OPT.<**> OPT.helper)
        ( OPT.fullDesc
            <> OPT.progDesc "Symbolic execution of programs in the QBE intermediate language"
        )
