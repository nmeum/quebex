-- SPDX-FileCopyrightText: 2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.Backend.QueryLogger (makeLogger) where

import Control.Exception.Base (IOException, throwIO)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import SimpleSMT qualified as SMT

data LoggerState
  = LoggerState
  { stSolver :: SMT.Solver,
    stDirectory :: FilePath,
    stPremable :: [SMT.SExpr],
    stCurData :: [SMT.SExpr],
    stNumQueries :: Int }

mkLoggerState :: SMT.Solver -> FilePath -> LoggerState
mkLoggerState solver directory
  = LoggerState
  { stSolver = solver,
    stDirectory = directory,
    stPremable = [],
    stCurData = [],
    stNumQueries = 0 }

addPreamble :: LoggerState -> SMT.SExpr -> LoggerState
addPreamble state expr =
  state { stPremable = stPremable state ++ [expr] }

addExpr' :: LoggerState -> SMT.SExpr -> LoggerState
addExpr' state expr = state { stCurData = stCurData state ++ [expr] }

addExpr :: LoggerState -> SMT.SExpr -> LoggerState
addExpr state expr@(SMT.List ((SMT.Atom "set-option"):_)) =
  addExpr' (addPreamble state expr) expr
addExpr state expr@(SMT.List ((SMT.Atom "set-logic"):_)) =
  addExpr' (addPreamble state expr) expr
addExpr state expr = addExpr' state expr

writeQuery :: LoggerState -> IO LoggerState
writeQuery state = do
  let qn = stNumQueries state
  let fp = stDirectory state </> "query" ++ show qn ++ ".smt2"
  writeFile fp (serialize $ stCurData state)
  pure $ state { stCurData = stPremable state, stNumQueries = qn + 1 }
 where
  serialize :: [SMT.SExpr] -> String
  serialize = unlines . map (\e -> SMT.showsSExpr e "")

------------------------------------------------------------------------

solverLogSend :: IORef LoggerState -> SMT.SExpr -> IO ()
solverLogSend stateRef (SMT.List [SMT.Atom "check-sat"]) = do
  st <- readIORef stateRef
  pc <- SMT.command (stSolver st) (SMT.List [SMT.Atom "get-assertions"])

  let assert = SMT.List [SMT.Atom "check-sat-assuming", pc]
  writeQuery (addExpr st assert) >>= writeIORef stateRef
solverLogSend _ (SMT.List ((SMT.Atom "push"):_)) = pure ()
solverLogSend _ (SMT.List ((SMT.Atom "pop"):_)) = pure ()
solverLogSend stateRef expr = do
  st <- readIORef stateRef
  writeIORef stateRef (addExpr st expr)

solverLogRecv :: SMT.SExpr -> IO ()
solverLogRecv _ = pure ()

solverLogExcn :: IOException -> IO ()
solverLogExcn = throwIO

solverLogStdErr :: String -> IO ()
solverLogStdErr = hPutStrLn stderr

makeLogger :: SMT.Solver -> FilePath -> IO SMT.SolverLogger
makeLogger solver dir = do
  createDirectoryIfMissing False dir
  stateRef <- newIORef (mkLoggerState solver dir)

  pure $ SMT.SolverLogger
    { SMT.solverLogSend = solverLogSend stateRef,
      SMT.solverLogRecv = solverLogRecv,
      SMT.solverLogExcn = solverLogExcn,
      SMT.solverLogStdErr = solverLogStdErr }
