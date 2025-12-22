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
import Language.QBE.Backend.ExecTree (BTree (..), ExecTree, addTrace, mkTree)
import Language.QBE.Backend.Model qualified as Model
import Language.QBE.Backend.Tracer (Branch (..), ExecTrace, solveTrace)
import SimpleBV qualified as SMT

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

-- Find an assignment that causes exploration of a new execution path through
-- the tested software. This function updates the metadata in the execution
-- tree and thus returns a new execution tree, even if no satisfiable
-- assignment was found.
findUnexplored :: SMT.Solver -> [SMT.SExpr] -> PathSel -> IO (Maybe Model.Model, PathSel)
findUnexplored solver inputVars tracer@(PathSel tree oldTrace) = do
  case negateBranch tree of
    Nothing -> pure (Nothing, tracer)
    Just nt -> do
      let nextTracer = PathSel (addTrace tree nt) nt
      res <- solveTrace solver inputVars oldTrace nt
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
        (++) [(True, br)] <$> negateBranch ifTrue
        <|> (++) [(False, br)] <$> negateBranch ifFalse
