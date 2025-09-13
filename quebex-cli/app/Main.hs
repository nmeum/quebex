-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Exception (Exception, throwIO)
import Data.List (find)
import Data.Maybe (isJust)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Explorer (defSolver, explore, logSolver, newEngine)
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.Symbolic.Unwind (unwind)
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT
import System.Directory (getTemporaryDirectory)
import System.IO (IOMode (WriteMode), hClose, hPutStr, openFile, openTempFile, withFile)
import Text.Parsec (ParseError)

data Opts = Opts
  { optMemStart :: MEM.Address,
    optMemSize :: MEM.Size,
    optLog :: Maybe FilePath,
    optLogIncr :: Maybe FilePath,
    optQBEFile :: FilePath
  }

optsParser :: OPT.Parser Opts
optsParser =
  Opts
    <$> OPT.option
      OPT.auto
      ( OPT.long "memory-start"
          <> OPT.short 'm'
          <> OPT.value 0x0
      )
    <*> OPT.option
      OPT.auto
      ( OPT.long "memory-size"
          <> OPT.short 's'
          <> OPT.value (1024 * 1024) -- 1 MB RAM
          <> OPT.help "Size of the memory region"
      )
    <*> OPT.optional
      ( OPT.strOption
          ( OPT.long "dump-smt2"
              <> OPT.short 'd'
              <> OPT.metavar "FILE"
              <> OPT.help "Output queries as a non-incremental SMT-LIB file"
          )
      )
    <*> OPT.optional
      ( OPT.strOption
          ( OPT.long "dump-incr-smt2"
              <> OPT.short 'D'
              <> OPT.metavar "FILE"
              <> OPT.help "Output queries as an incremental SMT-LIB file"
          )
      )
    <*> OPT.argument OPT.str (OPT.metavar "FILE")

------------------------------------------------------------------------

data ExecError
  = SyntaxError ParseError
  | UnknownFunction QBE.GlobalIdent
  deriving (Show)

instance Exception ExecError

exploreFile :: Opts -> IO [(ST.Assign, T.ExecTrace)]
exploreFile opts = do
  let filePath = optQBEFile opts
  content <- readFile filePath
  prog <- case parse filePath content of
    Right rt -> pure rt
    Left err -> throwIO $ SyntaxError err

  let funcs = globalFuncs prog
  func <- case find (\f -> QBE.fName f == entryFunc) funcs of
    Just x -> pure x
    Nothing -> throwIO $ UnknownFunction entryFunc

  env <- mkEnv prog (optMemStart opts) (optMemSize opts)
  if (isJust $ optLog opts) || (isJust $ optLogIncr opts)
    then do
      (logPath, logFile) <- case optLogIncr opts of
        Just fn -> do
          f <- openFile fn WriteMode
          pure (fn, f)
        Nothing -> do
          tempDir <- getTemporaryDirectory
          openTempFile tempDir "quebex-queries.smt2"

      ret <- exploreWithHandle env func logFile <* hClose logFile
      case optLog opts of
        Just fn -> withFile fn WriteMode (\h -> unwind logPath >>= hPutStr h)
        Nothing -> pure ()
      pure ret
    else do
      engine <- newEngine <$> defSolver
      explore engine env func params
  where
    params :: [(String, QBE.BaseType)]
    params = []

    entryFunc :: QBE.GlobalIdent
    entryFunc = QBE.GlobalIdent "main"

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
