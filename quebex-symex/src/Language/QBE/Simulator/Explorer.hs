-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
module Language.QBE.Simulator.Explorer
  ( defSolver,
    logSolver,
    Engine (expPathErr, expPathVars, expPathTrace),
    newEngine,
    explorePath,
    exploreFunc,
  )
where

import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, get, lift, modify, put)
import Data.Map qualified as Map
import Language.QBE.Backend.DFS (PathSel, findUnexplored, newPathSel, trackTrace)
import Language.QBE.Backend.Model (Model)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.State
  ( Env (envStore),
    ErrorPath (pathError, pathInput),
    ErrorState (errStore, errTracer),
    SimState (..),
    makeConcolic,
    runPath,
  )
import Language.QBE.Simulator.Error (EvalError)
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
    expPathErr :: Maybe EvalError,
    expPathVars :: ST.Assign,
    expPathTrace :: T.ExecTrace
  }

newEngine :: Env -> SMT.Solver -> Engine
newEngine env solver =
  Engine
    { expSolver = solver,
      expPathSel = newPathSel,
      expEnv = env,
      expPathErr = Nothing,
      expPathVars = Map.empty,
      expPathTrace = []
    }

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
explorePath :: SimState a -> StateT Engine IO Bool
explorePath simState = do
  engine@(Engine {expEnv = env}) <- get
  maybePath <- try $ run env
  let (mayErr, eTrace, nStore) =
        case maybePath of
          Left (err :: ErrorPath) ->
            let st = pathInput err
             in (Just $ pathError err, errTracer st, errStore st)
          Right (t, s) -> (Nothing, t, s)

  -- Before finalizing the store, we can extract the variables we encountered
  -- during this concrete execution, as well as the concrete values used for
  -- these variables during the execution.
  let inputVars = ST.sexprs nStore
      varAssign = ST.cValues nStore
  put $ engine {expPathErr = mayErr, expPathVars = varAssign, expPathTrace = eTrace}

  -- Finalize the store (declare new symbolic vars in solver) and then,
  -- based on the new solver state, solve constraints to find a new input.
  store <- liftIO $ ST.finalize (expSolver engine) nStore
  model <- findNext inputVars eTrace
  case model of
    Nothing -> pure False
    Just newModel -> do
      let nEnv = env {envStore = ST.setModel store newModel}
       in modify (\e -> e {expEnv = nEnv})
      pure True
  where
    run env = lift $ evalStateT (unSimState $ runPath simState) env

------------------------------------------------------------------------

exploreFunc ::
  Engine ->
  QBE.FuncDef ->
  [(String, QBE.ExtType)] ->
  IO [(ST.Assign, T.ExecTrace)]
exploreFunc engine entry params = do
  let funcState = mapM (uncurry makeConcolic) params >>= execFunc entry
  evalStateT (exploreFunc' funcState) engine
  where
    exploreFunc' st = do
      morePaths <- explorePath st
      curEngine <- get
      let ret = (expPathVars curEngine, expPathTrace curEngine)
       in if morePaths
            then (ret :) <$> exploreFunc' st
            else pure [ret]
