-- SPDX-FileCopyrightText: 2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.Backend.DFS
  ( PathSel,
    newPathSel,
    trackTrace,
    findUnexplored,
  )
where

import Control.Applicative ((<|>))
import Language.QBE.Backend (Model)
import Language.QBE.Backend.ExecTree (BTree (..), ExecTree, addTrace, mkTree)
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Simulator.Symbolic.Tracer (Branch (..), ExecTrace)
import SimpleSMT qualified as SMT

-- The 'PathSel' encapsulates data for the Dynamic Symbolic Execution (DSE)
-- algorithm. Specifically for path selection and incremental solving.
data PathSel
  = PathSel
      ExecTree -- The current execution tree for the DSE algorithm
      ExecTrace -- The last solved trace, for incremental solving.

-- Create a new empty 'PathSel' object without anything traced yet.
newPathSel :: PathSel
newPathSel = PathSel (mkTree []) []

-- Track a new 'ExecTrace' in the 'PathSel'.
trackTrace :: PathSel -> ExecTrace -> PathSel
trackTrace (PathSel tree t) trace =
  PathSel (addTrace tree trace) t

-- For a given execution trace, return an assignment (represented
-- as a 'Model') which statisfies all symbolic branch conditions.
-- If such an assignment does not exist, then 'Nothing' is returned.
solveTrace :: SMT.Solver -> [SMT.SExpr] -> PathSel -> ExecTrace -> IO (Maybe Model)
solveTrace solver vars (PathSel _ oldTrace) newTrace = do
  -- Determine the common prefix of the current trace and the old trace
  -- drop constraints beyond this common prefix from the current solver
  -- context. Thereby, keeping the common prefix and making use of
  -- incremental solving capabilities.
  let prefix = prefixLength newTrace oldTrace
  let toDrop = length oldTrace - prefix
  SMT.popMany solver $ fromIntegral toDrop

  -- Only enforce new constraints, i.e. those beyond the common prefix.
  assertTrace (drop prefix newTrace)

  isSat <- SMT.check solver
  case isSat of
    SMT.Sat -> Just <$> SMT.getExprs solver (vars)
    SMT.Unsat -> pure Nothing
    SMT.Unknown -> error "To-Do: Unknown Result" -- TODO
  where
    -- Add all conditions enforced by the given 'ExecTrace' to the solver.
    -- Returns a list of all asserted conditions.
    assertTrace :: ExecTrace -> IO ()
    assertTrace [] = pure ()
    assertTrace t = do
      let conds = map (\(b, Branch _ c) -> SE.toCond b c) t
      mapM_ (\c -> SMT.push solver >> SMT.assert solver c) conds

    -- Determine the length of the common prefix of two lists.
    --
    -- TODO: Move this elsewhere.
    prefixLength :: (Eq a) => [a] -> [a] -> Int
    prefixLength = prefixLength' 0
      where
        prefixLength' :: (Eq a) => Int -> [a] -> [a] -> Int
        prefixLength' n [] _ = n
        prefixLength' n _ [] = n
        prefixLength' n (x : xs) (y : ys)
          | x == y = prefixLength' (n + 1) xs ys
          | otherwise = n

-- Find an assignment that causes exploration of a new execution path through
-- the tested software. This function updates the metadata in the execution
-- tree and thus returns a new execution tree, even if no satisfiable
-- assignment was found.
--
-- TODO: Can we get by without passing 'inputVars` here again?
findUnexplored :: SMT.Solver -> [SMT.SExpr] -> PathSel -> IO (Maybe Model, PathSel)
findUnexplored solver inputVars tracer@(PathSel tree _) = do
  case negateBranch tree of
    Nothing -> pure (Nothing, tracer)
    Just nt -> do
      let nextTracer = PathSel (addTrace tree nt) nt
      res <- solveTrace solver inputVars tracer nt
      case res of
        Nothing -> findUnexplored solver inputVars nextTracer
        Just m -> pure (Just m, nextTracer)
  where
    -- Negate an unnegated branch in the execution tree and return an
    -- 'ExecTrace' which leads to an unexplored execution path. If no
    -- such path exists, then 'Nothing' is returned. If such a path
    -- exists a concrete variable assignment for it can be calculated
    -- using 'solveTrace'.
    --
    -- The branch node metadata in the resulting 'ExecTree' is updated
    -- to reflect that negation of the selected branch node was attempted.
    -- If further branches are to be negated, the resulting trace should
    -- be added to the 'ExecTree' using 'addTrace' to update the metadata
    -- in the tree as well.
    negateBranch :: ExecTree -> Maybe ExecTrace
    negateBranch Leaf = Nothing
    negateBranch (Node (Branch wasNeg ast) Nothing _)
      | wasNeg = Nothing
      | otherwise = Just [(True, Branch True ast)]
    negateBranch (Node (Branch wasNeg ast) _ Nothing)
      | wasNeg = Nothing
      | otherwise = Just [(False, Branch True ast)]
    negateBranch (Node br (Just ifTrue) (Just ifFalse)) =
      do
        -- TODO: Randomly prefer either the left or right child
        (++) [(True, br)] <$> negateBranch ifTrue
        <|> (++) [(False, br)] <$> negateBranch ifFalse
