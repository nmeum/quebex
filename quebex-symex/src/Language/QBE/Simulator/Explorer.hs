-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
module Language.QBE.Simulator.Explorer (explore, z3Solver) where

import Language.QBE (Program)
import Language.QBE.Backend.DFS (PathSel, findUnexplored, newPathSel, trackTrace)
import Language.QBE.Backend.Model (Model)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Concolic.State (run)
import Language.QBE.Simulator.Default.Expression qualified as DE
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

data Engine
  = Engine
  { expSolver :: SMT.Solver,
    expPathSel :: PathSel,
    expStore :: ST.Store
  }

newEngine :: IO Engine
newEngine = do
  solver <- z3Solver -- TODO: Make this configurable
  Engine solver newPathSel <$> ST.empty

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

------------------------------------------------------------------------

traceFunc :: Program -> QBE.FuncDef -> [CE.Concolic DE.RegVal] -> IO T.ExecTrace
traceFunc prog func params = run prog (execFunc func params)

explore :: Program -> QBE.FuncDef -> [(String, QBE.BaseType)] -> IO [(ST.Assign, T.ExecTrace)]
explore prog entryPoint entryParams = newEngine >>= explore'
  where
    explore' :: Engine -> IO [(ST.Assign, T.ExecTrace)]
    explore' engine@Engine {expSolver = solver, expStore = store} = do
      varAssign <- ST.assign store
      values <- mapM (uncurry (ST.getConcolic solver store)) entryParams
      eTrace <- traceFunc prog entryPoint values

      (model, nEngine) <- findNext engine eTrace
      case model of
        Nothing -> pure [(varAssign, eTrace)]
        Just _m -> (:) (varAssign, eTrace) <$> explore' nEngine
