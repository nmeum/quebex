-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Monad (void)
import Data.Word (Word8)
import Language.QBE.CmdLine qualified as CMD
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.State (Env, mkEnv, run)
import Options.Applicative qualified as OPT

execFile :: CMD.BasicArgs -> IO ()
execFile opts = do
  (prog, func) <- CMD.parseEntryFile $ CMD.optQBEFile opts

  env <- mkEnv prog (CMD.optMemStart opts) (CMD.optMemSize opts)
  void $ run (env :: Env DE.RegVal Word8) (execFunc func [])

main :: IO ()
main = OPT.execParser cmd >>= execFile
  where
    cmd :: OPT.ParserInfo CMD.BasicArgs
    cmd =
      OPT.info
        (CMD.basicArgs OPT.<**> OPT.helper)
        ( OPT.fullDesc
            <> OPT.progDesc "Concrete execution of programs in the QBE intermediate language"
        )
