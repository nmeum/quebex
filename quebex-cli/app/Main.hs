-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Exception (Exception, throwIO)
import Data.List (find)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Explorer (defSolver, explore, logSolver, newEngine)
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT
import System.IO (IOMode (WriteMode), withFile)
import Text.Parsec (ParseError)

data Opts = Opts
  { optDump :: Bool,
    optUnwind :: Bool,
    optSMTLog :: FilePath,
    optQBEFile :: FilePath
  }

optsParser :: OPT.Parser Opts
optsParser =
  Opts
    <$> OPT.switch
      ( OPT.long "dump-smt2"
          <> OPT.short 'd'
          <> OPT.help "Output queries as a non-incremental SMT-LIB file"
      )
    <*> OPT.switch
      ( OPT.long "dump-incr-smt2"
          <> OPT.short 'D'
          <> OPT.help "Output queries as an incremental SMT-LIB file"
      )
    <*> OPT.strOption
      ( OPT.long "log-file"
          <> OPT.short 'l'
          <> OPT.value "all-queries.smt2"
          <> OPT.metavar "FILE"
          <> OPT.help "Name of SMT-LIB file with generated queries"
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

  if (optDump opts || optUnwind opts)
    then withFile (optSMTLog opts) WriteMode (exploreWithHandle prog func)
    else do
      engine <- defSolver >>= newEngine
      explore engine prog func params
  where
    params :: [(String, QBE.BaseType)]
    params = []

    entryFunc :: QBE.GlobalIdent
    entryFunc = QBE.GlobalIdent "main"

    exploreWithHandle prog func handle = do
      engine <- logSolver handle >>= newEngine
      explore engine prog func params

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
