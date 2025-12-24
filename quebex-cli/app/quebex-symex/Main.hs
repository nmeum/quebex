-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.CmdLine qualified as CMD
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Explorer (defSolver, explore, logSolver, newEngine)
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT
import System.IO (IOMode (WriteMode), withFile)

data Opts = Opts
  { optLog :: Maybe FilePath,
    optSeed :: Maybe Int,
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

main :: IO ()
main = do
  args <- OPT.execParser cmd
  paths <- exploreFile args
  putStrLn $ "Amount of paths: " ++ show (length paths)
  where
    cmd :: OPT.ParserInfo Opts
    cmd =
      OPT.info
        (optsParser OPT.<**> OPT.helper)
        ( OPT.fullDesc
            <> OPT.progDesc "Symbolic execution of programs in the QBE intermediate language"
        )
