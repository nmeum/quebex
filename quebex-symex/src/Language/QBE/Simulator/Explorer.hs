-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
module Language.QBE.Simulator.Explorer
  ( newEngine,
    explore,
    defSolver,
    logSolver,
  )
where

import Language.QBE (Program)
import Language.QBE.Backend.DFS (PathSel, findUnexplored, newPathSel, trackTrace)
import Language.QBE.Backend.Model (Model)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Concolic.State (run)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import System.IO (Handle)

logic :: String
logic = "QF_BV"

defSolver :: IO SMT.Solver
defSolver = do
  -- l <- SMT.newLogger 0
  s <- SMT.newSolver "bitwuzla" [] Nothing
  SMT.setLogic s logic
  return s

logSolver :: Handle -> IO SMT.Solver
logSolver handle = do
  l <- SMT.newLoggerWithHandle handle 0
  s <-
    SMT.newSolverWithConfig
      (SMT.defaultConfig "bitwuzla" [])
        { SMT.solverLogger = SMT.smtSolverLogger l
        }
  SMT.setLogic s logic
  return s

------------------------------------------------------------------------

data Engine
  = Engine
  { expSolver :: SMT.Solver,
    expPathSel :: PathSel,
    expStore :: ST.Store
  }

newEngine :: SMT.Solver -> IO Engine
newEngine solver = Engine solver newPathSel <$> ST.empty

findNext :: Engine -> T.ExecTrace -> IO (Maybe Model, Engine)
findNext e@(Engine {expStore = store}) eTrace = do
  let pathSel = trackTrace (expPathSel e) eTrace
  symVars <- ST.sexprs store
  (model, nextPathSel) <- findUnexplored (expSolver e) symVars pathSel

  let ne = e {expPathSel = nextPathSel}
  case model of
    Nothing ->
      pure (model, ne)
    Just nm ->
      ST.setModel store nm >> pure (Just nm, ne)

traceFunc ::
  Program ->
  Engine ->
  QBE.FuncDef ->
  [CE.Concolic DE.RegVal] ->
  IO (T.ExecTrace, ST.Store)
traceFunc prog (Engine {expSolver = solver, expStore = store}) func params =
  run prog store solver (execFunc func params)

------------------------------------------------------------------------

explore ::
  Engine ->
  Program ->
  QBE.FuncDef ->
  [(String, QBE.BaseType)] ->
  IO [(ST.Assign, T.ExecTrace)]
explore engine@Engine {expSolver = solver, expStore = store} prog entry params = do
  varAssign <- ST.assign store
  values <- mapM (uncurry (ST.getConcolic solver store)) params

  (eTrace, nStore) <- traceFunc prog engine entry values
  (model, nEngine) <- findNext (engine {expStore = nStore}) eTrace
  case model of
    Nothing -> pure [(varAssign, eTrace)]
    Just _m -> (:) (varAssign, eTrace) <$> explore nEngine prog entry params
