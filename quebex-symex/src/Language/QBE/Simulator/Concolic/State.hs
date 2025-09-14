-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Concolic.State
  ( Env (..),
    mkEnv,
    run,
    makeConcolic,
  )
where

import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, StateT, gets, modify, runStateT)
import Data.Map qualified as Map
import Data.Word (Word8)
import Language.QBE (Program)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.State qualified as DS
import Language.QBE.Simulator.Error (EvalError (FuncArgsMismatch, TypingError))
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.State
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE

data Env
  = Env
  { envBase :: DS.Env (CE.Concolic DE.RegVal) (CE.Concolic Word8),
    envTracer :: T.ExecTrace,
    envStore :: ST.Store
  }

mkEnv ::
  Program ->
  MEM.Address ->
  MEM.Size ->
  IO Env
mkEnv prog memStart memSize = do
  initEnv <- DS.mkEnv prog memStart memSize
  Env initEnv T.newExecTrace <$> ST.empty

liftState ::
  (DS.SimState (CE.Concolic DE.RegVal) (CE.Concolic Word8)) a ->
  (StateT Env IO) a
liftState toLift = do
  defEnv <- gets envBase
  (a, s) <- liftIO $ runStateT toLift defEnv
  modify (\ps -> ps {envBase = s})
  pure a

makeConcolic :: String -> QBE.BaseType -> StateT Env IO (CE.Concolic DE.RegVal)
makeConcolic name ty = do
  st <- gets envStore
  let (ns, cv) = ST.getConcolic st name ty
  modify (\e -> e {envStore = ns})
  pure cv

modifyTracer :: (MonadState Env m) => (T.ExecTrace -> T.ExecTrace) -> m ()
modifyTracer f =
  modify (\s@Env {envTracer = t} -> s {envTracer = f t})

makeSymbolicWord ::
  QBE.GlobalIdent ->
  [CE.Concolic DE.RegVal] ->
  StateT Env IO (Maybe (CE.Concolic DE.RegVal))
-- TODO: Require a string as a parameter.
makeSymbolicWord _ [ident] = do
  v <- makeConcolic ("word" ++ show (E.toWord64 ident)) QBE.Word
  pure $ Just v
makeSymbolicWord ident _ = throwM $ FuncArgsMismatch ident

findSimFunc :: QBE.GlobalIdent -> Maybe ([CE.Concolic DE.RegVal] -> (StateT Env IO) (Maybe (CE.Concolic DE.RegVal)))
findSimFunc i@(QBE.GlobalIdent "make_symbolic_word") = Just (makeSymbolicWord i)
findSimFunc _ = Nothing

instance Simulator (StateT Env IO) (CE.Concolic DE.RegVal) where
  isTrue value = do
    let condResult = E.toWord64 (CE.concrete value) /= 0
    case CE.symbolic value of
      Nothing -> pure condResult
      Just sexpr -> do
        -- Track the taken branch in the tracer.
        let branch = T.newBranch sexpr
        modifyTracer (\t -> T.appendBranch t condResult branch)

        pure condResult

  -- Implements address concretization as a memory model.
  toAddress CE.Concolic {CE.concrete = cv, CE.symbolic = svMaybe} =
    case svMaybe of
      Just sv ->
        case sv `E.eq` SE.fromReg cv of
          Just c -> do
            modifyTracer (`T.appendCons` c)
            pure $ E.toWord64 cv
          Nothing -> throwM TypingError
      Nothing -> pure $ E.toWord64 cv

  findFunc ident = do
    funcs <- gets (DS.envFuncs . envBase)
    pure $ case Map.lookup ident funcs of
      Just x -> Just $ SFuncDef x
      Nothing -> SSimFunc <$> findSimFunc ident

  lookupSymbol = liftState . lookupSymbol
  activeFrame = liftState activeFrame
  pushStackFrame = liftState . pushStackFrame
  popStackFrame = liftState popStackFrame
  getSP = liftState getSP
  setSP = liftState . setSP

  writeMemory a t v = liftState (writeMemory a t v)
  readMemory t a = liftState (readMemory t a)

------------------------------------------------------------------------

run :: Env -> StateT Env IO a -> IO (T.ExecTrace, ST.Store)
run env state = fst <$> runStateT go env
  where
    go = do
      _ <- state
      t <- gets envTracer
      s <- gets envStore
      pure (t, s)
