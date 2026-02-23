-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Data.Word (Word64, Word8)
import Language.QBE.CmdLine qualified as CMD
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.State (Env, mkEnv, run)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)

fromWord :: DE.RegVal -> Maybe Word64
fromWord v
  | E.getType v == QBE.Base QBE.Word = Just $ E.toWord64 v
  | otherwise = Nothing

execFile :: CMD.BasicArgs -> IO Int
execFile opts = do
  (prog, func) <- CMD.parseEntryFile $ CMD.optQBEFile opts

  env <- mkEnv prog (CMD.optMemStart opts) (CMD.optMemSize opts)
  res <- run (env :: Env DE.RegVal Word8) (execFunc func [])
  case res >>= fromWord of
    Just x -> pure $ fromIntegral x
    Nothing ->
      error $ "expected '" ++ show CMD.entryFunc ++ "' to return an int"

main :: IO ()
main = do
  retVal <- OPT.execParser cmd >>= execFile
  exitWith $
    if retVal == 0
      then ExitSuccess
      else ExitFailure retVal
  where
    cmd :: OPT.ParserInfo CMD.BasicArgs
    cmd =
      OPT.info
        (CMD.basicArgs OPT.<**> OPT.helper)
        ( OPT.fullDesc
            <> OPT.progDesc "Concrete execution of programs in the QBE intermediate language"
        )
