-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Monad.State (evalStateT, gets, liftIO)
import Data.Binary (encodeFile)
import Data.KTest (KTest (KTest), KTestObj, fromAssign)
import Data.String (fromString)
import Language.QBE.CmdLine qualified as CMD
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Explorer
  ( Engine (expPathVars),
    defSolver,
    explorePath,
    logSolver,
    newEngine,
  )
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

data KTestConf = KTestConf FilePath String
  deriving (Show)

mkKTestConf :: FilePath -> String -> IO KTestConf
mkKTestConf directory name = do
  createDirectoryIfMissing True directory
  pure $ KTestConf directory name

writeKTest :: KTestConf -> Int -> [KTestObj] -> IO ()
writeKTest (KTestConf directory name) pathID =
  writeKTest' pathID . KTest [fromString name]
  where
    writeKTest' :: Int -> KTest -> IO ()
    writeKTest' n ktest = do
      flip encodeFile ktest $
        addExtension
          (directory </> ("test" ++ printf "%06d" n))
          ".ktest"

------------------------------------------------------------------------

exploreEntry :: Maybe KTestConf -> Engine -> QBE.FuncDef -> IO Int
exploreEntry ktest engine entry =
  evalStateT (go 1 $ execFunc entry []) engine
  where
    go n st = do
      morePaths <- explorePath st
      case ktest of
        Just conf -> do
          assign <- gets (fromAssign . expPathVars)
          liftIO $ writeKTest conf n assign
        Nothing -> pure ()

      if morePaths
        then go (n + 1) st
        else pure n

exploreFile :: Opts -> IO Int
exploreFile opts@Opts {optBase = base} = do
  (prog, func) <- CMD.parseEntryFile $ CMD.optQBEFile base

  ktest <-
    case optTests opts of
      Just dir -> do
        Just <$> mkKTestConf dir (CMD.optQBEFile $ optBase opts)
      Nothing -> pure Nothing

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
