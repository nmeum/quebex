-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Util where

import Data.Bifunctor (second)
import Data.List (find)
import Data.Word (Word64)
import Language.QBE (Program, globalFuncs, parse)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Concolic.State (mkEnv, run)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Explorer (defSolver, explore, newEngine)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleBV qualified as SMT

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
  env <- mkEnv prog 0 128
  fst <$> run env (execFunc func params)

unconstrained :: SMT.Solver -> Word64 -> String -> QBE.BaseType -> IO (CE.Concolic DE.RegVal)
unconstrained solver initCon name ty = do
  let symbolic = SE.fromSExpr ty $ SMT.const name (QBE.baseTypeBitSize ty)

  -- TODO: remove this
  let bits = SMT.tBits $ fromIntegral (QBE.baseTypeBitSize ty)
  _ <- SMT.declare solver name bits

  let concrete = E.fromLit (QBE.Base ty) initCon
  pure $ CE.Concolic concrete (Just symbolic)

explore' :: Program -> QBE.FuncDef -> [(String, QBE.BaseType)] -> IO [(ST.Assign, T.ExecTrace)]
explore' prog entry params = do
  engine <- newEngine <$> defSolver
  defEnv <- mkEnv prog 0 128
  explore engine defEnv entry $
    map (second QBE.Base) params
