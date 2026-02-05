-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
-- SPDX-FileCopyrightText: 2026 Reliable System Software, Technische Universität Braunschweig <vss@ibr.cs.tu-bs.de>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Analysis.CFG
  ( CFG,
    Label,
    cfgEdges,
    basicBlockToLabel,
    labelToBasicBlock,
    lookupSuccessors,
    build,
    cfgToGraph,
    cfgToRooted,
  )
where

import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (singleton)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Language.QBE.Analysis.Graph qualified as G
import Language.QBE.Types qualified as QBE

type Label = IntMap.Key

-- TODO: Need to figure this mess wrt. data types: Should this point to a Block
-- or a BlockIdent or a custom Block data structure which includes both? Hmhm…
data CFG
  = CFG
  { cfgFunction :: QBE.FuncDef,
    cfgLabelMap :: Map.Map QBE.BlockIdent Label,
    cfgBlockMap :: IntMap.IntMap QBE.BlockIdent,
    cfgSuccessors :: IntMap.IntMap Successors -- TODO: Use a Set or List here?
  }
  deriving (Show)

cfgEdges :: CFG -> [(Label, Label)]
cfgEdges cfg@(CFG {cfgSuccessors = succs}) =
  foldr (\l acc -> getSuccessEdges l ++ acc) [] $
    IntMap.keys (cfgBlockMap cfg)
  where
    getSuccessEdges :: Label -> [(Label, Label)]
    getSuccessEdges l =
      let labelSuccs = fromJust $ IntMap.lookup l succs
       in map (l,) (successorsToBlockList' labelSuccs)

basicBlockToLabel :: CFG -> QBE.BlockIdent -> Maybe Label
basicBlockToLabel CFG {cfgLabelMap = m} blkId = Map.lookup blkId m

labelToBasicBlock :: CFG -> Label -> Maybe QBE.BlockIdent
labelToBasicBlock CFG {cfgBlockMap = m} label = IntMap.lookup label m

lookupSuccessors :: CFG -> QBE.BlockIdent -> Maybe [QBE.BlockIdent]
lookupSuccessors cfg ident = do
  label <- basicBlockToLabel cfg ident
  successorsToBlockList cfg <$> IntMap.lookup label (cfgSuccessors cfg)

------------------------------------------------------------------------

-- A basic block can have one unconditional successor or two possible successors
-- in the case of a conditional jump. In QBE, there can never be more than two.
data Successors
  = SuccNone
  | SuccUncond Label
  | SuccCond Label Label
  deriving (Show, Eq)

successorsToBlockList' :: Successors -> [Label]
successorsToBlockList' SuccNone = []
successorsToBlockList' (SuccUncond label) = singleton label
successorsToBlockList' (SuccCond ifT ifF) = [ifT, ifF]

successorsToBlockList :: CFG -> Successors -> [QBE.BlockIdent]
successorsToBlockList cfg succs = map getBlock (successorsToBlockList' succs)
  where
    getBlock :: Label -> QBE.BlockIdent
    getBlock label = fromJust $ IntMap.lookup label (cfgBlockMap cfg)

successorsToIntSet :: Successors -> IntSet.IntSet
successorsToIntSet = IntSet.fromList . successorsToBlockList'

------------------------------------------------------------------------

haltIdent :: (QBE.BlockIdent, Label)
haltIdent = (QBE.BlockIdent "=halt", 0)

returnIdent :: (QBE.BlockIdent, Label)
returnIdent = (QBE.BlockIdent "=return", 1)

-- Keep in sync with 'haltIdent' and 'returnIdent'.
identStart :: Label
identStart = 2

build' :: Map.Map QBE.BlockIdent Label -> [QBE.Block] -> [(IntMap.Key, Successors)]
build' labelMap = foldl go [(snd haltIdent, SuccNone), (snd returnIdent, SuccNone)]
  where
    getId :: QBE.BlockIdent -> Label
    getId ident = fromJust $ Map.lookup ident labelMap

    go acc block@(QBE.Block {QBE.label = ident}) =
      let succs = case QBE.term block of
            QBE.Jump target -> SuccUncond (getId target)
            QBE.Jnz _ i1 i2 -> SuccCond (getId i1) (getId i2)
            QBE.Return _ -> SuccUncond $ snd returnIdent
            QBE.Halt -> SuccUncond $ snd haltIdent
       in (getId ident, succs) : acc

build :: QBE.FuncDef -> CFG
build func =
  CFG
    { cfgFunction = func,
      cfgLabelMap = labelMap,
      cfgBlockMap = IntMap.fromList $ map swap blkIdLabels,
      cfgSuccessors = IntMap.fromList $ build' labelMap blocks
    }
  where
    labelMap :: Map.Map QBE.BlockIdent Label
    labelMap = Map.fromList blkIdLabels

    blocks :: [QBE.Block]
    blocks = QBE.fBlock func

    blkIdLabels :: [(QBE.BlockIdent, Label)]
    blkIdLabels = [haltIdent, returnIdent] ++ zip (map QBE.label blocks) [identStart ..]

------------------------------------------------------------------------

cfgToGraph :: CFG -> G.Graph
cfgToGraph cfg@(CFG {cfgLabelMap = labelMap}) =
  IntMap.fromList $ map (\l -> (l, succSet l)) (Map.elems labelMap)
  where
    succSet :: Label -> IntSet.IntSet
    succSet l =
      successorsToIntSet
        (fromJust $ IntMap.lookup l (cfgSuccessors cfg))

cfgToRooted :: CFG -> G.Rooted
cfgToRooted cfg = (identStart, cfgToGraph cfg)
