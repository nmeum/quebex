-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Concolic.State
  ( Env (..),
    mkEnv,
    run,
    runPath,
    makeConcolic,
    ErrorState (..),
    ErrorPath (..),
    SimState (..),
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
  ( MonadState,
    StateT (StateT),
    evalStateT,
    get,
    gets,
    modify,
    runStateT,
  )
import Data.Map qualified as Map
import Data.Word (Word8)
import Language.QBE (Program)
import Language.QBE.Backend.Store qualified as ST
import Language.QBE.Backend.Tracer qualified as T
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.Funcs (lookupSimFunc)
import Language.QBE.Simulator.Default.State qualified as DS
import Language.QBE.Simulator.Error (EvalError (FuncArgsMismatch, TypingError))
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.State
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import System.Random (initStdGen, mkStdGen)

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
  Maybe Int ->
  IO Env
mkEnv prog memStart memSize maySeed = do
  initEnv <- DS.mkEnv prog memStart memSize
  randGen <-
    case maySeed of
      Just sd -> pure $ mkStdGen sd
      Nothing -> initStdGen
  pure $ Env initEnv T.newExecTrace (ST.empty randGen)

liftState ::
  (DS.SimState (CE.Concolic DE.RegVal) (CE.Concolic Word8)) a ->
  SimState a
liftState (DS.SimState toLift) = do
  defEnv <- gets envBase
  (a, s) <- liftIO $ runStateT toLift defEnv
  modify (\ps -> ps {envBase = s})
  pure a

makeConcolic :: String -> QBE.ExtType -> SimState (CE.Concolic DE.RegVal)
makeConcolic name ty = do
  st <- gets envStore
  let (ns, cv) = ST.getConcolic st name ty
  modify (\e -> e {envStore = ns})
  pure cv

modifyTracer :: (MonadState Env m) => (T.ExecTrace -> T.ExecTrace) -> m ()
modifyTracer f =
  modify (\s@Env {envTracer = t} -> s {envTracer = f t})

makeSymbolicArray ::
  QBE.GlobalIdent ->
  [CE.Concolic DE.RegVal] ->
  SimState (Maybe (CE.Concolic DE.RegVal))
makeSymbolicArray _ [arrayPtr, numElem, elemSize, namePtr] = do
  name <- E.toString <$> (toAddress namePtr >>= readNullArray)
  vlty <- case E.toWord64 elemSize of
    1 -> pure QBE.Byte
    2 -> pure QBE.HalfWord
    4 -> pure (QBE.Base QBE.Word)
    8 -> pure (QBE.Base QBE.Long)
    _ -> throwError TypingError

  values <-
    mapM
      (\n -> makeConcolic (name ++ show n) vlty)
      [1 .. E.toWord64 numElem]

  arrayAddr <- toAddress arrayPtr
  liftState (DS.SimState $ DS.storeValues arrayAddr values) >> pure Nothing
makeSymbolicArray ident _ = throwError $ FuncArgsMismatch ident

findSimFunc :: QBE.GlobalIdent -> Maybe ([CE.Concolic DE.RegVal] -> SimState (Maybe (CE.Concolic DE.RegVal)))
findSimFunc i@(QBE.GlobalIdent "quebex_make_symbolic") = Just (makeSymbolicArray i)
findSimFunc ident = lookupSimFunc ident

------------------------------------------------------------------------

-- | State of the concolic executor with which an error was triggered
-- in the application code, which can be reproduced using this state.
data ErrorState
  = ErrorState
  { errTracer :: T.ExecTrace,
    errStore :: ST.Store
  }

-- | Exception thrown upon encountered an 'ErrorState'.
data ErrorPath
  = ErrorPath
  { pathInput :: ErrorState,
    pathError :: EvalError
  }

instance Exception ErrorPath

instance Show ErrorPath where
  show (ErrorPath _ err) = show err

------------------------------------------------------------------------

newtype SimState a = SimState {unSimState :: StateT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

deriving instance MonadState Env SimState

-- Implements 'MonadError' in 'SimState' via 'IOException's. On throw,
-- it also returns the relevant executor state by encapsulting it in
-- an 'ErrorPath'.
--
-- See also: The instance for 'DS.SimState'.
instance MonadError EvalError SimState where
  throwError err = do
    Env {envTracer = t, envStore = s} <- get
    liftIO $ throwIO (ErrorPath (ErrorState t s) err)

  catchError (SimState st) handler =
    SimState $ DS.unliftCatch st (unSimState . handler)

------------------------------------------------------------------------

instance Simulator SimState (CE.Concolic DE.RegVal) where
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
          Nothing -> throwError TypingError
      Nothing -> pure $ E.toWord64 cv

  findFunc ident = do
    funcs <- gets (DS.envFuncs . envBase)
    pure $ case Map.lookup ident funcs of
      Just x -> Just $ SFuncDef x
      Nothing -> SSimFunc <$> findSimFunc ident
  findFuncByAddr addr = do
    fptrs <- gets (DS.envFuncAddrs . envBase)
    case Map.lookup addr fptrs of
      Just fn -> findFunc fn
      Nothing -> pure Nothing

  lookupSymbol = liftState . lookupSymbol
  activeFrame = liftState activeFrame
  pushStackFrame = liftState . pushStackFrame
  popStackFrame = liftState popStackFrame
  getSP = liftState getSP
  setSP = liftState . setSP

  writeMemory a t v = liftState (writeMemory a t v)
  readMemory t a = liftState (readMemory t a)

------------------------------------------------------------------------

runPath :: SimState a -> SimState (T.ExecTrace, ST.Store)
runPath state = do
  _ <- state
  t <- gets envTracer
  s <- gets envStore
  pure (t, s)

run :: Env -> SimState a -> IO (T.ExecTrace, ST.Store)
run env state = evalStateT (unSimState $ runPath state) env
