-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Symbolic (explore, z3Solver) where

import Control.Exception (throwIO)
import Control.Monad.State (gets)
import Language.QBE (Program)
import Language.QBE.Backend (Model)
import Language.QBE.Backend.DFS (PathSel, findUnexplored, newPathSel, trackTrace)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Simulator (execFunc, runExec)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Error (EvalError)
import Language.QBE.Simulator.State (envTracer)
import Language.QBE.Simulator.Symbolic.Tracer qualified as T
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT

z3Solver :: IO SMT.Solver
z3Solver = do
  -- l <- SMT.newLogger 0
  s <- SMT.newSolver "z3" ["-in", "-smt2"] Nothing
  SMT.setLogic s "QF_BV"
  return s

------------------------------------------------------------------------

data ExpEngine
  = ExpEngine
  { expSolver :: SMT.Solver,
    expPathSel :: PathSel,
    expStore :: ST.Store
  }

newEngine :: IO ExpEngine
newEngine = do
  solver <- z3Solver -- TODO: Make this configurable
  ExpEngine solver newPathSel <$> ST.empty

findNext :: ExpEngine -> T.ExecTrace -> IO (Maybe Model, ExpEngine)
findNext e eTrace = do
  let pathSel = trackTrace (expPathSel e) eTrace
  (model, nextPathSel) <- findUnexplored (expSolver e) pathSel

  let ne = e {expPathSel = nextPathSel}
  case model of
    Nothing ->
      pure (model, ne)
    Just nm ->
      ST.setModel (expStore e) nm >> pure (Just nm, ne)

------------------------------------------------------------------------

traceFunc :: Program -> QBE.FuncDef -> [CE.Concolic] -> IO (Either EvalError T.ExecTrace)
traceFunc prog func params =
  runExec prog (execFunc func params >> gets envTracer) T.newExecTrace

explore :: Program -> QBE.FuncDef -> [(String, QBE.BaseType)] -> IO [T.ExecTrace]
explore prog entryPoint entryParams = newEngine >>= explore'
  where
    explore' :: ExpEngine -> IO [T.ExecTrace]
    explore' engine@ExpEngine {expSolver = solver, expStore = store} = do
      values <- mapM (uncurry (ST.getConcolic solver store)) entryParams
      eTrace <- traceFunc prog entryPoint values >>= either throwIO pure

      (model, nEngine) <- findNext engine eTrace
      case model of
        Nothing -> pure [eTrace]
        Just _m -> (:) eTrace <$> explore' nEngine
