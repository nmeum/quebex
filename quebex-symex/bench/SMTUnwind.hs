-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module SMTUnwind (unwind) where

import Control.Monad (forM_)
import Control.Monad.State (State, gets, modify, runState)
import Data.Functor ((<&>))
import SimpleSMT qualified as SMT

data UnwindEnv
  = UnwindEnv
  { assertStack :: [[SMT.SExpr]],
    exprs :: [SMT.SExpr],
    queries :: [[SMT.SExpr]]
  }
  deriving (Show, Eq)

mkUnwindEnv :: UnwindEnv
mkUnwindEnv = UnwindEnv [] [] []

------------------------------------------------------------------------

newAssertLevel :: State UnwindEnv ()
newAssertLevel =
  modify (\s -> s {assertStack = [] : assertStack s})

popAssertLevel :: State UnwindEnv ()
popAssertLevel = modify go
  where
    go s@UnwindEnv {assertStack = []} = s
    go s@UnwindEnv {assertStack = _ : xs} = s {assertStack = xs}

addAssertion :: [SMT.SExpr] -> State UnwindEnv ()
addAssertion assertions = do
  stk <- gets assertStack
  let newStk = case stk of
        (x : xs) -> (x ++ assertions) : xs
        [] -> [assertions]
  modify (\s -> s {assertStack = newStk})

addExpr :: SMT.SExpr -> State UnwindEnv ()
addExpr expr =
  modify (\s -> s {exprs = exprs s ++ [expr]})

getAsserts :: State UnwindEnv [SMT.SExpr]
getAsserts = gets (concat . assertStack)

completeQuery :: State UnwindEnv ()
completeQuery =
  modify
    ( \s ->
        s
          { queries = queries s ++ [exprs s],
            exprs = []
          }
    )

transExpr :: SMT.SExpr -> State UnwindEnv ()
transExpr (SMT.List [SMT.Atom "push", SMT.Atom arg]) = do
  let num = (read arg :: Integer)
  forM_ [1 .. num] (const newAssertLevel)
transExpr (SMT.List [SMT.Atom "pop", SMT.Atom arg]) = do
  let num = (read arg :: Integer)
  forM_ [1 .. num] (const popAssertLevel)
transExpr (SMT.List ((SMT.Atom "assert") : xs)) =
  addAssertion xs
transExpr (SMT.List [SMT.Atom "check-sat"]) = do
  asserts <- getAsserts
  addExpr $ SMT.List [SMT.Atom "check-sat-assuming", SMT.List asserts]
  completeQuery
transExpr (SMT.List ((SMT.Atom "get-value") : _)) = pure ()
transExpr expr = addExpr expr

transform :: [SMT.SExpr] -> State UnwindEnv ()
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
    runTransform e = runState (transform e) mkUnwindEnv

unwind :: FilePath -> IO String
unwind origFp = do
  exprs <- readFile origFp <&> readSExprs
  let queries = getQueries exprs
  pure $ serialize (concat queries)
  where
    serialize :: [SMT.SExpr] -> String
    serialize = unlines . map (`SMT.showsSExpr` "")
