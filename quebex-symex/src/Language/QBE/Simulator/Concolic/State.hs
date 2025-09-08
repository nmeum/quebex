-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Concolic.State (run) where

import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, StateT, gets, modify, runStateT)
import Data.Word (Word8)
import Language.QBE (Program, globalFuncs)
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.State qualified as DS
import Language.QBE.Simulator.Error (EvalError (TypingError))
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.State
import Language.QBE.Simulator.Symbolic.Expression qualified as SE

data Env
  = Env
  { envBase :: DS.Env (CE.Concolic DE.RegVal) (CE.Concolic Word8),
    envTracer :: T.ExecTrace
  }

liftState ::
  (DS.SimState (CE.Concolic DE.RegVal) (CE.Concolic Word8)) a ->
  (StateT Env IO) a
liftState toLift = do
  defEnv <- gets envBase
  (a, s) <- liftIO $ runStateT toLift defEnv
  modify (\ps -> ps {envBase = s})
  pure a

modifyTracer :: (MonadState Env m) => (T.ExecTrace -> T.ExecTrace) -> m ()
modifyTracer f =
  modify (\s@Env {envTracer = t} -> s {envTracer = f t})

instance Simulator (StateT Env IO) (CE.Concolic DE.RegVal) where
  condBranch CE.Concolic {CE.symbolic = Just sexpr} condResult = do
    let branch = T.newBranch sexpr
    modifyTracer (\t -> T.appendBranch t condResult branch)
  condBranch _ _ = pure ()

  -- Implements address concretization as a memory model.
  toAddress CE.Concolic {CE.concrete = cv, CE.symbolic = svMaybe} =
    case svMaybe of
      Just sv ->
        case sv `E.eq` (SE.fromReg cv) of
          Just c -> do
            modifyTracer (\t -> T.appendCons t c)
            pure $ E.toWord64 cv
          Nothing -> throwM TypingError
      Nothing -> pure $ E.toWord64 cv

  lookupGlobal = liftState . lookupGlobal
  findFunc = liftState . findFunc

  activeFrame = liftState activeFrame
  pushStackFrame = liftState . pushStackFrame
  popStackFrame = liftState popStackFrame
  getSP = liftState getSP
  setSP = liftState . setSP

  writeMemory a t v = liftState (writeMemory a t v)
  readMemory t a = liftState (readMemory t a)

------------------------------------------------------------------------

run :: Program -> StateT Env IO a -> IO T.ExecTrace
run prog state = do
  initEnv' <- liftIO $ DS.mkEnv (globalFuncs prog) 0x0 128 -- TODO
  fst <$> runStateT (state >> gets envTracer) (Env initEnv' T.newExecTrace)
