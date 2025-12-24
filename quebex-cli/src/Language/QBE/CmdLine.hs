-- SPDX-FileCopyrightText: 2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.CmdLine
  ( BasicArgs (..),
    basicArgs,
    entryFunc,
    parseEntryFile,
  )
where

import Language.QBE (Program, parseAndFind)
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE
import Options.Applicative qualified as OPT

-- | t'BasicArgs' can be combined/extended with additional parsers using
-- the '<*>' applicative operator provided by "Options.Applicative".
data BasicArgs = BasicArgs
  { -- | Start address of the general-purpose memory.
    optMemStart :: MEM.Address,
    -- | Size of the memory in bytes.
    optMemSize :: MEM.Size,
    -- | Path to the QBE input file.
    optQBEFile :: FilePath
  }

-- | "Options.Applicative" parser for t'BasicArgs'.
basicArgs :: OPT.Parser BasicArgs
basicArgs =
  BasicArgs
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
    <*> OPT.argument OPT.str (OPT.metavar "FILE")

------------------------------------------------------------------------

-- | Name of the entry function.
entryFunc :: QBE.GlobalIdent
entryFunc = QBE.GlobalIdent "main"

-- | Parse a file and find the 'entryFunc'.
parseEntryFile :: FilePath -> IO (Program, QBE.FuncDef)
parseEntryFile filePath =
  readFile filePath >>= parseAndFind entryFunc
