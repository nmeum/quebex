-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Monad (when)
import Control.Monad.State.Strict (evalStateT, gets, liftIO)
import Data.Binary (encodeFile)
import Data.KTest (KTest (KTest), KTestObj, fromAssign)
import Data.String (fromString)
import Language.QBE.Backend.Store (Assign)
import Language.QBE.CmdLine qualified as CMD
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Error (EvalError)
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
import System.Exit (die)
import System.FilePath (addExtension, (</>))
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withFile)
import Text.Printf (printf)

data Opts = Opts
  { optLog :: Maybe FilePath,
    optSeed :: Maybe Int,
    optTestDir :: Maybe FilePath,
    optErrExit :: Bool,
    optWriteAll :: Bool,
    optVerbose :: Bool,
    optBase :: CMD.BasicArgs
  }

optTestCases :: String
optTestCases = "test-cases"

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
          ( OPT.long optTestCases
              <> OPT.short 't'
              <> OPT.metavar "FILE"
              <> OPT.help "Directory to write generate test inputs to"
          )
      )
    <*> OPT.switch
      ( OPT.long "exit-on-error"
          <> OPT.short 'e'
          <> OPT.help "Stop exploration after encountering the first error"
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

writeAssign :: Maybe KTestConf -> LogLevel -> Int -> Assign -> IO ()
writeAssign Nothing _ _ _ = pure ()
writeAssign (Just conf) level pathID assign
  | level >= confLevel conf = writeKTest conf pathID (fromAssign assign)
  | otherwise = pure ()

testCasePath :: KTestConf -> Int -> FilePath
testCasePath (KTestConf {confPath = directory}) n =
  addExtension
    (directory </> ("test" ++ printf "%06d" n))
    ".ktest"

writeKTest :: KTestConf -> Int -> [KTestObj] -> IO ()
writeKTest conf@(KTestConf {confName = name}) pathID =
  writeKTest' pathID . KTest [fromString name]
  where
    writeKTest' :: Int -> KTest -> IO ()
    writeKTest' n ktest = do
      flip encodeFile ktest $
        testCasePath conf n

------------------------------------------------------------------------

handleError :: Opts -> Maybe KTestConf -> Int -> EvalError -> IO ()
handleError opts ktest n err = do
  printErr
  when (optErrExit opts) $
    die "Exiting due to encountered error"
  where
    printErr = do
      hPutStrLn stderr $
        "Encountered error on path #"
          ++ show n
          ++ ": "
          ++ show err
          ++ "\n"
          ++ "↳ "
          ++ printPath ktest

    printPath :: Maybe KTestConf -> String
    printPath Nothing = "Pass --" ++ optTestCases ++ " to generate test case"
    printPath (Just kt) =
      "Refer to the KTest file in " ++ show (testCasePath kt n)

exploreEntry :: Opts -> Maybe KTestConf -> Engine -> QBE.FuncDef -> IO Int
exploreEntry opts ktest engine entry =
  evalStateT (go 1 $ execFunc entry []) engine
  where
    go n st = do
      when (optVerbose opts) $
        liftIO (putStrLn $ "Exploring path " ++ show n ++ "...")
      morePaths <- explorePath st

      lastPath <- gets expLastPath
      logLevel <- case pathErr lastPath of
        Just err -> liftIO $ do
          handleError opts ktest n err
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
  ktest <-
    case optTestDir opts of
      Just dir -> do
        Just <$> mkKTestConf logLevel dir binName
      Nothing -> pure Nothing

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
