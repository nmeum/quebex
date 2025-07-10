-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Exec (exec) where

import Control.Monad (unless)
import Data.List (find)
import Data.Word (Word8)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator (execFunc, runExec)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression as E
import Language.QBE.Simulator.State (Exec)
import Language.QBE.Simulator.Tracer as T
import Language.QBE.Types qualified as QBE

entryFunc :: QBE.GlobalIdent
entryFunc = QBE.GlobalIdent "entry"

exec :: [D.RegVal] -> String -> IO ()
exec params input = do
  program <- case parse "input" input of
    Left err -> fail $ "parsing error: " ++ show err
    Right pr -> pure pr

  let funcs = globalFuncs program
  func <- case find (\f -> QBE.fName f == entryFunc) funcs of
    Just x -> pure x
    Nothing -> fail $ "unknown function: " ++ show entryFunc

  result <- runExec program (execFunc func params :: Exec D.RegVal Word8 T.NoOp (Maybe D.RegVal)) T.NoOp
  returnValue <- case result of
    Left err -> fail $ "evaluation error: " ++ show err
    Right v -> pure v

  case returnValue of
    Just x ->
      unless (E.isZero x) $
        fail "non-zero exit status"
    Nothing -> fail $ show entryFunc ++ " must return a value"
