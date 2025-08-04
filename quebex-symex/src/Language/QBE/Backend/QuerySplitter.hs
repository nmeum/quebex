-- SPDX-FileCopyrightText: 2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.Backend.QuerySplitter where

import Control.Monad (forM_)
import Control.Monad.State (State, gets, modify, runState)
import Data.Functor ((<&>))
import SimpleSMT qualified as SMT
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data TransEnv
  = TransEnv
  { assertStack :: [[SMT.SExpr]],
    preamble :: [SMT.SExpr],
    exprs :: [SMT.SExpr],
    queries :: [[SMT.SExpr]]
  }
  deriving (Show, Eq)

mkTransEnv :: TransEnv
mkTransEnv = TransEnv [] [] [] []

------------------------------------------------------------------------

newAssertLevel :: State TransEnv ()
newAssertLevel =
  modify (\s -> s {assertStack = assertStack s ++ []})

popAssertLevel :: State TransEnv ()
popAssertLevel = modify go
  where
    go s@TransEnv {assertStack = []} = s
    go s@TransEnv {assertStack = _ : xs} = s {assertStack = xs}

addAssertion :: [SMT.SExpr] -> State TransEnv ()
addAssertion assertions = do
  stk <- gets assertStack
  let newStk = case stk of
        (x : _) -> [x ++ assertions]
        _ -> [assertions]
  modify (\s -> s {assertStack = newStk})

addExpr :: SMT.SExpr -> State TransEnv ()
addExpr expr =
  modify (\s -> s {exprs = exprs s ++ [expr]})

getAsserts :: State TransEnv [SMT.SExpr]
getAsserts = concat <$> gets assertStack

addPreamble :: SMT.SExpr -> State TransEnv ()
addPreamble expr =
  modify (\s -> s {preamble = preamble s ++ [expr]})

completeQuery :: State TransEnv ()
completeQuery =
  modify
    ( \s ->
        s
          { queries = queries s ++ [preamble s ++ exprs s],
            exprs = []
          }
    )

transExpr :: SMT.SExpr -> State TransEnv ()
transExpr (SMT.List [SMT.Atom "push", SMT.Atom arg]) = do
  let num = (read arg :: Integer)
  forM_ [0 .. num] (\_ -> newAssertLevel) -- TODO: ntimes
transExpr (SMT.List [SMT.Atom "pop", SMT.Atom arg]) = do
  let num = (read arg :: Integer)
  forM_ [0 .. num] (\_ -> popAssertLevel) -- TODO: ntimes
transExpr (SMT.List ((SMT.Atom "assert") : xs)) =
  addAssertion xs
transExpr (SMT.List [SMT.Atom "check-sat"]) = do
  asserts <- getAsserts
  addExpr $ SMT.List [SMT.Atom "check-sat-assuming", SMT.List asserts]
  completeQuery
-- TODO: declare-*
-- TODO: define-*
transExpr expr@(SMT.List ((SMT.Atom "declare-fun") : _)) = do
  addPreamble expr
transExpr expr@(SMT.List ((SMT.Atom "set-option") : _)) = do
  addPreamble expr
transExpr expr@(SMT.List ((SMT.Atom "set-logic") : _)) = do
  addPreamble expr
transExpr (SMT.List ((SMT.Atom "get-value") : _)) = pure ()
transExpr expr = addExpr expr

transform :: [SMT.SExpr] -> State TransEnv ()
transform sexprs = forM_ sexprs transExpr

------------------------------------------------------------------------

readSExprs :: String -> [SMT.SExpr]
readSExprs str = go (SMT.readSExpr str)
  where
    go :: Maybe (SMT.SExpr, String) -> [SMT.SExpr]
    go Nothing = []
    go (Just (acc, rest)) = acc : go (SMT.readSExpr rest)

getQueries :: [SMT.SExpr] -> [[SMT.SExpr]]
getQueries exprs = queries (snd $ runTransform exprs)
  where
    runTransform e = runState (transform e) mkTransEnv

writeQuery :: FilePath -> Int -> [SMT.SExpr] -> IO ()
writeQuery dfp qn q = do
  let fp = dfp </> "query" ++ show qn ++ ".smt2"
  writeFile fp (serialize q)
  where
    serialize :: [SMT.SExpr] -> String
    serialize = unlines . map (\e -> SMT.showsSExpr e "")

splitQueries :: FilePath -> FilePath -> IO ()
splitQueries qfp dfp = do
  exprs <- readFile qfp <&> readSExprs
  let queries = getQueries exprs
  createDirectoryIfMissing False dfp
  forM_ (zip queries [0 ..]) (\(q, n) -> writeQuery dfp n q)
