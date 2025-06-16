module Language.QBE.Analysis.CFG (BlockMap, build) where

import Data.List (singleton)
import Data.Map qualified as Map
import Language.QBE.Types qualified as QBE

type BlockMap = Map.Map QBE.BlockIdent [QBE.BlockIdent]

returnIdent :: QBE.BlockIdent
returnIdent = QBE.BlockIdent "=return"

haltIdent :: QBE.BlockIdent
haltIdent = QBE.BlockIdent "=halt"

------------------------------------------------------------------------

build' :: [QBE.Block] -> [(QBE.BlockIdent, [QBE.BlockIdent])]
build' = foldl go []
  where
    go acc block@(QBE.Block {QBE.label = ident}) =
      let succs = case QBE.term block of
            QBE.Jump target -> singleton target
            QBE.Jnz _ i1 i2 -> [i1, i2]
            QBE.Return _ -> singleton returnIdent
            QBE.Halt -> singleton haltIdent
       in (ident, succs) : acc

-- TODO: Return a product type which, among other things, contains the BlockMap.
-- TODO: Detect loops via dominators.
build :: QBE.FuncDef -> BlockMap
build func = Map.fromList (build' $ QBE.fBlock func)
