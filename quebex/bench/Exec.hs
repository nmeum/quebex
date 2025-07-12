-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Exec (exec) where

import Control.Monad (void)
import Data.List (find)
import Data.Word (Word8)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Default.State (SimState, run)
import Language.QBE.Types qualified as QBE

entryFunc :: QBE.GlobalIdent
entryFunc = QBE.GlobalIdent "entry"

exec :: [D.RegVal] -> String -> IO ()
exec params input = do
  prog <- case parse "input" input of
    Left err -> fail $ "parsing error: " ++ show err
    Right pr -> pure pr

  let funcs = globalFuncs prog
  func <- case find (\f -> QBE.fName f == entryFunc) funcs of
    Just x -> pure x
    Nothing -> fail $ "unknown function: " ++ show entryFunc

  void $ run prog (execFunc func params :: SimState D.RegVal Word8 (Maybe D.RegVal))
