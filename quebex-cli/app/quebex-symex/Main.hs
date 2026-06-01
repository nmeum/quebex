-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Monad (unless, when)
import Control.Monad.State (evalStateT, gets, liftIO)
import Data.Binary (encodeFile)
import Data.KTest (KTest (KTest), KTestObj, fromAssign)
import Data.String (fromString)
import Language.QBE.Backend.Store (Assign)
import Language.QBE.CmdLine qualified as CMD
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Explorer
  ( Engine (expLastPath),
    PathResult (pathErr, pathVars),
    defSolver,
    explorePath,
    logSolver,
    newEngine,
  )
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (addExtension, (</>))
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withFile)
import Text.Printf (printf)

data Opts = Opts
  { optLog :: Maybe FilePath,
    optSeed :: Maybe Int,
    optTestDir :: FilePath,
    optContinue :: Bool,
    optWriteAll :: Bool,
    optVerbose :: Bool,
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
      ( OPT.long "test-cases"
          <> OPT.short 't'
          <> OPT.metavar "FILE"
          <> OPT.value "quebex-results"
          <> OPT.help "Directory to write generate test inputs to"
      )
    <*> OPT.switch
      ( OPT.long "continue-on-error"
          <> OPT.short 'c'
          <> OPT.help "Continue exploration after encoutering an error path"
      )
    <*> OPT.switch
      ( OPT.long "write-all"
          <> OPT.short 'a'
          <> OPT.help "Write tests for all paths, not just those with errors"
      )
    <*> OPT.switch
      ( OPT.long "verbose"
          <> OPT.short 'v'
          <> OPT.help "Enable more verbose output"
      )
    <*> CMD.basicArgs

------------------------------------------------------------------------

data LogLevel = LogAll | LogErr
  deriving (Show, Eq, Ord)

data KTestConf
  = KTestConf
  { confLevel :: LogLevel,
    confPath :: FilePath,
    confName :: String
  }
  deriving (Show)

mkKTestConf :: LogLevel -> FilePath -> String -> IO KTestConf
mkKTestConf level directory name = do
  createDirectoryIfMissing True directory
  pure $ KTestConf level directory name

writeAssign :: KTestConf -> LogLevel -> Int -> Assign -> IO ()
writeAssign conf level pathID assign
  | level >= confLevel conf = writeKTest conf pathID (fromAssign assign)
  | otherwise = pure ()

writeKTest :: KTestConf -> Int -> [KTestObj] -> IO ()
writeKTest KTestConf {confPath = directory, confName = name} pathID =
  writeKTest' pathID . KTest [fromString name]
  where
    writeKTest' :: Int -> KTest -> IO ()
    writeKTest' n ktest = do
      flip encodeFile ktest $
        addExtension
          (directory </> ("test" ++ printf "%06d" n))
          ".ktest"

------------------------------------------------------------------------

exploreEntry :: Opts -> KTestConf -> Engine -> QBE.FuncDef -> IO Int
exploreEntry opts ktest engine entry =
  evalStateT (go 1 $ execFunc entry []) engine
  where
    printErr n err = do
      hPutStrLn stderr $
        "Encoundered error on path #"
          ++ show n
          ++ ": "
          ++ show err
          ++ "\n"
          ++ "↳ Check the generated .ktest file in "
          ++ show (confPath ktest)

    go n st = do
      when (optVerbose opts) $
        liftIO (putStrLn $ "Exploring path " ++ show n ++ "...")
      morePaths <- explorePath st

      lastPath <- gets expLastPath
      logLevel <- case pathErr lastPath of
        Just err -> liftIO $ do
          printErr n err
          unless (optContinue opts) exitFailure
          pure LogErr
        Nothing -> pure LogAll

      liftIO $ writeAssign ktest logLevel n (pathVars lastPath)
      if morePaths
        then go (n + 1) st
        else pure n

exploreFile :: Opts -> IO Int
exploreFile opts@Opts {optBase = base} = do
  (prog, func) <- CMD.parseEntryFile $ CMD.optQBEFile base

  let binName = CMD.optQBEFile $ optBase opts
      logLevel = if optWriteAll opts then LogAll else LogErr
  ktest <- mkKTestConf logLevel (optTestDir opts) binName

  env <- mkEnv prog (CMD.optMemStart base) (CMD.optMemSize base) (optSeed opts)
  case optLog opts of
    Just fn -> withFile fn WriteMode (exploreWithHandle ktest env func)
    Nothing -> do
      engine <- newEngine env <$> defSolver
      exploreEntry opts ktest engine func
  where
    exploreWithHandle ktest env func handle = do
      engine <- newEngine env <$> logSolver handle
      exploreEntry opts ktest engine func

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
