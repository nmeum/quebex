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

import Language.QBE.Backend.DFS (PathSel, findUnexplored, newPathSel, trackTrace)
import Language.QBE.Backend.Model (Model)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Concolic.State (Env (envStore), run')
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
    expPathSel :: PathSel
  }

newEngine :: SMT.Solver -> Engine
newEngine solver = Engine solver newPathSel

findNext :: Engine -> [SMT.SExpr] -> T.ExecTrace -> IO (Maybe Model, Engine)
findNext e symVars eTrace = do
  let pathSel = trackTrace (expPathSel e) eTrace
  (model, nextPathSel) <- findUnexplored (expSolver e) symVars pathSel

  let ne = e {expPathSel = nextPathSel}
  case model of
    Nothing ->
      pure (model, ne)
    Just nm ->
      pure (Just nm, ne)

------------------------------------------------------------------------

traceFunc :: Env -> QBE.FuncDef -> [CE.Concolic DE.RegVal] -> IO T.ExecTrace
traceFunc env func params = run' env (execFunc func params)

explore ::
  Engine ->
  Env ->
  QBE.FuncDef ->
  [(String, QBE.BaseType)] ->
  IO [(ST.Assign, T.ExecTrace)]
explore engine@(Engine {expSolver = solver}) env entry params = do
  let store = envStore env
  varAssign <- ST.assign store
  values <- mapM (uncurry (ST.getConcolic solver store)) params
  eTrace <- traceFunc env entry values

  inputVars <- ST.sexprs store
  (model, nEngine) <- findNext engine inputVars eTrace
  case model of
    Nothing -> pure [(varAssign, eTrace)]
    Just newModel -> do
      ST.setModel store newModel
      (:) (varAssign, eTrace) <$> explore nEngine env entry params
