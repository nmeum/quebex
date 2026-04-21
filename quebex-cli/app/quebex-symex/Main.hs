-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Monad.Catch (catch)
import Control.Monad.State (StateT, evalStateT, gets, liftIO)
import Language.QBE.CmdLine qualified as CMD
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Error (EvalError)
import Language.QBE.Simulator.Explorer
  ( Engine (expPathVars),
    defSolver,
    explorePath,
    logSolver,
    newEngine,
  )
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withFile)
import System.Log.KTest (LogConf (..), LogLevel (..), logAssign, mkLogger)

data Opts = Opts
  { optLog :: Maybe FilePath,
    optSeed :: Maybe Int,
    optTestDir :: FilePath,
    optWriteAll :: Bool,
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
    <*> OPT.strOption
      ( OPT.long "results"
          <> OPT.short 'r'
          <> OPT.metavar "DIR"
          <> OPT.value "quebex-results/"
          <> OPT.help "Directory to write generate test files to"
      )
    <*> OPT.switch
      ( OPT.long "write-all"
          <> OPT.short 'a'
          <> OPT.help "Write tests for all paths, not just those with errors"
      )
    <*> CMD.basicArgs

------------------------------------------------------------------------

exploreEntry :: LogConf -> Engine -> QBE.FuncDef -> IO Int
exploreEntry ktest engine entry =
  evalStateT (go 1 $ execFunc entry []) engine
  where
    logTest l n =
      gets expPathVars >>= liftIO . logAssign ktest l n

    go n st = do
      morePaths <- catch (explorePath st) (handleExp n)
      logTest LogAll n

      if morePaths
        then go (n + 1) st
        else pure n

    handleExp :: Int -> EvalError -> StateT Engine IO Bool
    handleExp n e = do
      liftIO $
        hPutStrLn stderr $
          "Encoundered error on path #"
            ++ show n
            ++ ": "
            ++ show e
            ++ "\n"
            ++ "↳ Check the generated .ktest file in "
            ++ show (confPath ktest)

      logTest LogErr n
      pure False

exploreFile :: Opts -> IO Int
exploreFile opts@Opts {optBase = base} = do
  (prog, func) <- CMD.parseEntryFile $ CMD.optQBEFile base

  let binName = CMD.optQBEFile $ optBase opts
      logLevel = if optWriteAll opts then LogAll else LogErr
  ktest <- mkLogger logLevel (optTestDir opts) binName

  env <- mkEnv prog (CMD.optMemStart base) (CMD.optMemSize base) (optSeed opts)
  case optLog opts of
    Just fn -> withFile fn WriteMode (exploreWithHandle ktest env func)
    Nothing -> do
      engine <- newEngine env <$> defSolver
      exploreEntry ktest engine func
  where
    exploreWithHandle ktest env func handle = do
      engine <- newEngine env <$> logSolver handle
      exploreEntry ktest engine func

------------------------------------------------------------------------

cmd :: OPT.ParserInfo Opts
cmd =
  OPT.info
    (optsParser OPT.<**> OPT.helper)
    ( OPT.fullDesc
        <> OPT.progDesc "Symbolic execution of programs in the QBE intermediate language"
    )

main :: IO ()
main = do
  numPaths <- OPT.execParser cmd >>= exploreFile
  putStrLn $ "\n---\nAmount of paths: " ++ show numPaths
