-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Util where

import Data.Bifunctor (second)
import Data.Word (Word64)
import Language.QBE (parseAndFind)
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

parseAndExec :: QBE.GlobalIdent -> [CE.Concolic DE.RegVal] -> String -> IO ST.ExecTrace
parseAndExec funcName params input = do
  (prog, entry) <- parseAndFind funcName input

  env <- mkEnv prog 0 128 Nothing
  fst <$> run env (execFunc entry params)

unconstrained :: SMT.Solver -> Word64 -> String -> QBE.BaseType -> IO (CE.Concolic DE.RegVal)
unconstrained solver initCon name ty = do
  symbolic <- SMT.declareBV solver name $ QBE.baseTypeBitSize ty
  let concrete = E.fromLit (QBE.Base ty) initCon
  pure $ CE.Concolic concrete (Just $ SE.fromSExpr symbolic)

explore' :: String -> String -> [(String, QBE.BaseType)] -> IO [(ST.Assign, T.ExecTrace)]
explore' input funcName params = do
  (prog, entry) <- parseAndFind (QBE.GlobalIdent funcName) input

  engine <- newEngine <$> defSolver
  defEnv <- mkEnv prog 0 128 Nothing
  explore engine defEnv entry $
    map (second QBE.Base) params
