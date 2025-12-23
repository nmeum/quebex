-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator (parseAndFind)
import Language.QBE.Simulator.Concolic.State (mkEnv)
import Language.QBE.Simulator.Explorer (defSolver, explore, logSolver, newEngine)
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT
import System.IO (IOMode (WriteMode), withFile)

data Opts = Opts
  { optMemStart :: MEM.Address,
    optMemSize :: MEM.Size,
    optLog :: Maybe FilePath,
    optSeed :: Maybe Int,
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
    <*> OPT.argument OPT.str (OPT.metavar "FILE")

------------------------------------------------------------------------

exploreFile :: Opts -> IO [(ST.Assign, T.ExecTrace)]
exploreFile opts = do
  let filePath = optQBEFile opts
  (prog, func) <- readFile filePath >>= parseAndFind entryFunc

  env <- mkEnv prog (optMemStart opts) (optMemSize opts) (optSeed opts)
  case optLog opts of
    Just fn -> withFile fn WriteMode (exploreWithHandle env func)
    Nothing -> do
      engine <- newEngine <$> defSolver
      explore engine env func params
  where
    params :: [(String, QBE.ExtType)]
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
