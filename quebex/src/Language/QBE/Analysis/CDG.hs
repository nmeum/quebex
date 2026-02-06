-- SPDX-FileCopyrightText: 2010 Tristan Ravitch <travitch@csCFG.cfgReturnRoot cfg.wisc.edu>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-only

-- Based on the implementation provided by LLVM.Analysis.CDG from Tristan Ravitch
-- See https://hackage.haskell.org/package/llvm-analysis-0.3.0/docs/src/LLVM-Analysis-CDG.html
--
-- The implementation by Tristan Ravitch mentions a paper by Cytron et al.
-- See: https://doi.org/10.1145/115372.115320
--
-- However, I found that the original paper by Ferrante et al. does a much better job at
-- explaining what was implemented by Tristan Ravitch in llvm-analysis. Hence, the comments
-- below mainly refer to that: https://doi.org/10.1145/24039.24041
module Language.QBE.Analysis.CDG (CDG, computeCDG) where

import Data.Bifunctor (second)
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.List (find)
import Data.Maybe (fromMaybe)
import Language.QBE.Analysis.CFG qualified as CFG
import Language.QBE.Analysis.Graph qualified as G

type CDG = M.IntMap S.IntSet

computeCDG :: CFG.CFG -> M.IntMap S.IntSet
computeCDG cfg =
  -- From the CFG, generate a post-dominator tree and also convert this tree
  -- to an IntMap representation for efficient successor lookup in 'addCDGEdge'.
  let rooted = CFG.cfgReturnRoot cfg
      pdTree = G.pdomTree rooted
      pdtMap = M.fromList $ map (second S.fromList) (G.pdom rooted)
      pdtAnc = M.fromList (G.ancestors pdTree)
   in foldr (uncurry $ addCDGEdge pdtMap pdtAnc) M.empty $ CFG.cfgEdges cfg

-- This function essentially implements the algorithm described in Section 3.1
-- of the Paper by Ferrante et al., using the algorithm by Cytron et al. may be
-- more efficient and could be considered in the future.
addCDGEdge ::
  M.IntMap S.IntSet ->
  M.IntMap [Int] ->
  CFG.Label ->
  CFG.Label ->
  M.IntMap S.IntSet ->
  M.IntMap S.IntSet
addCDGEdge pdtMap pdtAnc a b acc
  -- Consider all edges (A, B) in the control flow graph such that B does not
  -- post-dominate M. If it does, we return 'acc' unmodified (insert nothing).
  | postdominates b a = acc
  | otherwise =
      -- Let AC denote the least common ancestor of A and B in the post-dominator tree.
      case commonAncestor b a of
        -- Case 1: All nodes in the post-dominator tree on the path from AC to
        -- B, including B but not AC, should be made control dependent on A.
        Just ac ->
          let cdepsOnA = S.insert b (S.filter (/= ac) $ lookupSucc b)
           in foldr insertEdge acc (S.toList cdepsOnA)
        -- Case 2: All nodes in the post-dominator tree on the path from A to B,
        -- including A and B, should be made control dependent on A.
        Nothing ->
          let deps = S.insert b $ lookupSucc b
           in foldr insertEdge acc (S.toList deps)
  where
    insertEdge :: CFG.Label -> M.IntMap S.IntSet -> M.IntMap S.IntSet
    insertEdge blk = M.insertWith S.union blk (S.singleton a)

    lookupSucc :: CFG.Label -> S.IntSet
    lookupSucc l = fromMaybe S.empty $ M.lookup l pdtMap

    -- Returns true if 'x' post-dominates 'y'.
    postdominates :: CFG.Label -> CFG.Label -> Bool
    postdominates x y = maybe False (x `S.member`) $ M.lookup y pdtMap

    commonAncestor :: G.Node -> G.Node -> Maybe G.Node
    commonAncestor n1 n2 = do
      a1 <- M.lookup n1 pdtAnc
      a2 <- M.lookup n2 pdtAnc
      find (`elem` a1) a2
