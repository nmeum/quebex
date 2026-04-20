-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
module Language.QBE.Simulator.Explorer
  ( newEngine,
    hasNext,
    explorePath,
    explore,
    defSolver,
    logSolver,
  )
where

import Control.Monad.State
import Language.QBE.Backend.DFS (PathSel, findUnexplored, newPathSel, trackTrace)
import Language.QBE.Backend.Model (Model)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.State (Env (envStore), makeConcolic, runPath)
import Language.QBE.Types qualified as QBE
import SimpleBV qualified as SMT
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
    expEnv :: Env,
    expNext :: Bool
  }

newEngine :: Env -> SMT.Solver -> Engine
newEngine env solver = Engine solver newPathSel env True

findNext :: [SMT.SExpr] -> T.ExecTrace -> StateT Engine IO (Maybe Model)
findNext symVars eTrace = do
  engine <- get

  let pathSel = trackTrace (expPathSel engine) eTrace
  (model, nextPathSel) <-
    liftIO $ findUnexplored (expSolver engine) symVars pathSel

  put $ engine {expPathSel = nextPathSel}
  pure model

-- TODO: Consider modelling changes of the PathSel (via findNext) and
-- changes of the Store (via ST.finalize and ST.setModel) as a StateT.
explorePath :: StateT Env IO a -> StateT Engine IO (ST.Assign, T.ExecTrace)
explorePath simState = do
  engine <- get

  let env = expEnv engine
  (eTrace, nStore) <- lift $ evalStateT (runPath simState) env

  -- Before finalizing the store, we can extract the variables we encountered
  -- during this concrete execution, as well as the concrete values used for
  -- these variables during the execution.
  let inputVars = ST.sexprs nStore
      varAssign = ST.cValues nStore
  finalStore <- liftIO $ ST.finalize (expSolver engine) nStore

  model <- findNext inputVars eTrace
  case model of
    Nothing -> do
      modify (\e -> e {expNext = False})
      pure (varAssign, eTrace)
    Just newModel -> do
      let nEnv = env {envStore = ST.setModel finalStore newModel}
       in modify (\e -> e {expEnv = nEnv})
      pure (varAssign, eTrace)

hasNext :: StateT Engine IO Bool
hasNext = gets expNext

------------------------------------------------------------------------

explore ::
  Engine ->
  QBE.FuncDef ->
  [(String, QBE.ExtType)] ->
  IO [(ST.Assign, T.ExecTrace)]
explore engine entry params = do
  let funcState = mapM (uncurry makeConcolic) params >>= execFunc entry
  evalStateT (explore' funcState) engine
  where
    explore' st = do
      res <- explorePath st
      morePaths <- hasNext
      if morePaths
        then (res :) <$> explore' st
        else pure [res]
