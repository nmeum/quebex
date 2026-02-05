-- SPDX-FileCopyrightText: 2010 Tristan Ravitch <travitch@cs.wisc.edu>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: BSD-3-Clause AND GPL-3.0-only

-- Based on the implementation provided by LLVM.Analysis.CDG from Tristan Ravitch
-- See https://hackage.haskell.org/package/llvm-analysis-0.3.0/docs/src/LLVM-Analysis-CDG.html
module Language.QBE.Analysis.CDG where

import Data.Bifunctor (second)
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Tree qualified as T
import Language.QBE.Analysis.CFG qualified as CFG
import Language.QBE.Analysis.Graph qualified as G

-- data CDG = CDG (T.Tree Node) (M.IntMap S.IntSet)

computeCDG :: CFG.CFG -> M.IntMap S.IntSet
computeCDG cfg =
  -- From the CFG, generate a post-dominator tree and also convert this tree
  -- to an IntMap representation for efficient successor lookup in 'addCDGEdge'.
  let rooted = CFG.cfgReturnRoot cfg
      pdTree = G.pdomTree rooted
      pdtMap = M.fromList $ map (second S.fromList) (G.pdom rooted)
   in foldr (uncurry $ addCDGEdge pdTree pdtMap) M.empty $ CFG.cfgEdges cfg

-- computeCDG :: CFG -> M.IntMap S.IntSet
-- computeCDG cfg@(CFG { cfgSuccessors = succs }) =
--   let rooted = CFG.cfgToRooted cfg
--       pdTree = G.pdomTree rooted
--       pdtMap = M.fromList $ map (\(n, p) -> (n, S.fromList p)) (G.pdom rooted)
--    in foldr addPairs 2 M.mempty
--   where
--     addPairs :: CFG.Label -> M.IntMap S.IntSet -> M.IntMap S.IntSet
--     addPairs b acc =
--       foldr (addCDGEdge pdTree pdtMap b) acc (fromJust $ Map.lookup b succs)

addCDGEdge ::
  T.Tree G.Node ->
  M.IntMap S.IntSet ->
  CFG.Label ->
  CFG.Label ->
  M.IntMap S.IntSet ->
  M.IntMap S.IntSet
addCDGEdge pdt pdtMap m n acc
  | postdominates n m = acc
  | otherwise =
      case commonAncestor pdt n m of
        Just ac ->
          -- All nodes in the post-dominator tree on the path from AC to M,
          -- including M but not AC, should be made control dependent on N.
          let cdepsOnM = S.insert n (S.filter (/= ac) (fromJust $ M.lookup n pdtMap))
           in foldr insertEdge acc (S.toList cdepsOnM)
        -- If there is no common ancestor, then all of the postdominators
        -- of N are control dependent on M.
        Nothing ->
          let deps = S.insert n (fromJust $ M.lookup n pdtMap)
           in foldr insertEdge acc (S.toList deps)
  where
    insertEdge :: CFG.Label -> M.IntMap S.IntSet -> M.IntMap S.IntSet
    insertEdge blk = M.insertWith S.union blk (S.singleton blk)

    postdominates :: CFG.Label -> CFG.Label -> Bool
    postdominates x y = y `S.member` fromJust (M.lookup x pdtMap)

    commonAncestor :: T.Tree G.Node -> G.Node -> G.Node -> Maybe G.Node
    commonAncestor t n1 n2 =
      let mp = M.fromList (G.ancestors t) -- TODO
          a1 = fromJust $ M.lookup n1 mp
          a2 = fromJust $ M.lookup n2 mp
       in find (`elem` a1) a2
