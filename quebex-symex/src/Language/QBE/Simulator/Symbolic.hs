module Language.QBE.Simulator.Symbolic (explore) where

import Control.Monad (void)
import Control.Monad.State (gets)
import Language.QBE (Program)
import Language.QBE.Simulator (execFunc, runExec)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Error (EvalError)
import Language.QBE.Simulator.State (envTracer)
import Language.QBE.Simulator.Symbolic.Store qualified as ST
import Language.QBE.Simulator.Symbolic.Tracer qualified as T
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT

z3Solver :: IO SMT.Solver
z3Solver = do
  -- l <- SMT.newLogger 0
  s <- SMT.newSolver "z3" ["-in", "-smt2"] Nothing
  SMT.setLogic s "QF_BV"
  void $ SMT.check s
  return s

------------------------------------------------------------------------

data ExpEngine
  = ExpEngine
  { expSolver :: SMT.Solver,
    expPathSel :: T.PathSel
  }

newEngine :: IO ExpEngine
newEngine = do
  s <- z3Solver -- TODO: Make this configurable
  pure $ ExpEngine s T.newPathSel

nextStore :: ExpEngine -> ST.Store -> T.ExecTrace -> IO (Maybe ST.Store, ExpEngine)
nextStore e store eTrace = do
  let pathSel = T.trackTrace (expPathSel e) eTrace
  (model, newPathSel) <- T.findUnexplored (expSolver e) pathSel

  let ne = e {expPathSel = newPathSel}
  case model of
    Nothing ->
      pure (Nothing, ne)
    Just nm -> do
      case ST.setModel store nm of
        Nothing -> error "invalid model" -- TODO
        Just st -> pure (Just st, ne)

------------------------------------------------------------------------

traceFunc :: Program -> QBE.FuncDef -> [CE.Concolic] -> IO (Either EvalError T.ExecTrace)
traceFunc prog func params =
  runExec prog (execFunc func params >> gets envTracer) T.newExecTrace

explore :: Program -> QBE.FuncDef -> [(String, QBE.BaseType)] -> IO [T.ExecTrace]
explore prog entryPoint entryParams = do
  exEng <- newEngine
  ST.empty >>= explore' exEng
  where
    explore' :: ExpEngine -> ST.Store -> IO [T.ExecTrace]
    explore' engine@ExpEngine {expSolver = solver} store = do
      values <- mapM (uncurry (ST.getConcolic solver store)) entryParams
      -- TODO: Proper error handling for traceFunc
      (Right eTrace) <- traceFunc prog entryPoint values

      (nStore, nEngine) <- nextStore engine store eTrace
      case nStore of
        Nothing -> pure [eTrace]
        Just st -> (:) eTrace <$> explore' nEngine st
