-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Util where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Word (Word64)
import Language.QBE (Program, globalFuncs, parse)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Concolic.State (Env (..), run)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.State qualified as DS
import Language.QBE.Simulator.Explorer (defSolver, explore, newEngine)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT

defaultEnv :: Program -> IO Env
defaultEnv prog = do
  initEnv' <- liftIO $ DS.mkEnv (globalFuncs prog) 0x0 128 -- TODO
  initStore <- ST.empty
  pure $ Env initEnv' T.newExecTrace initStore

parseProg :: String -> IO Program
parseProg input =
  case parse "" input of
    Left e -> fail $ "Unexpected parsing error: " ++ show e
    Right r -> pure r

findFunc :: Program -> QBE.GlobalIdent -> QBE.FuncDef
findFunc prog funcName =
  case find (\f -> QBE.fName f == funcName) (globalFuncs prog) of
    Just x -> x
    Nothing -> error $ "Unknown function: " ++ show funcName

-- TODO: Code duplication with quebex/test/Simulator.hs
parseAndExec :: QBE.GlobalIdent -> [CE.Concolic DE.RegVal] -> String -> IO ST.ExecTrace
parseAndExec funcName params input = do
  prog <- parseProg input
  let func = findFunc prog funcName
  run prog (execFunc func params)

unconstrained :: SMT.Solver -> Word64 -> String -> QBE.BaseType -> IO (CE.Concolic DE.RegVal)
unconstrained solver initCon name ty = do
  let concrete = E.fromLit ty initCon
  symbolic <- SE.symbolic solver name ty
  pure $ CE.Concolic concrete (Just symbolic)

explore' :: Program -> QBE.FuncDef -> [(String, QBE.BaseType)] -> IO [(ST.Assign, T.ExecTrace)]
explore' prog entry params = do
  engine <- newEngine <$> defSolver
  defEnv <- defaultEnv prog
  explore engine defEnv entry params
