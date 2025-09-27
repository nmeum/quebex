-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Backend.Store
  ( Store (cValues),
    Assign,
    empty,
    sexprs,
    finalize,
    setModel,
    getConcolic,
  )
where

import Data.Map qualified as Map
import Language.QBE.Backend.Model qualified as Model
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleBV qualified as SMT
import System.Random (StdGen, genWord64, initStdGen)

-- | Concrete variable assignment.
type Assign = Map.Map String DE.RegVal

-- A variable store mapping variable names to concrete values.
data Store
  = Store
  { cValues :: Assign,
    sValues :: Map.Map String SE.BitVector,
    defined :: Map.Map String SE.BitVector,
    randGen :: StdGen
  }

-- | Create a new (empty) store.
empty :: IO Store
empty =
  -- TODO: Make seed configurable
  Store Map.empty Map.empty Map.empty <$> initStdGen

-- | Obtain symbolic values as a list of 'SimpleBV' expressions.
sexprs :: Store -> [SMT.SExpr]
sexprs = map SE.toSExpr . Map.elems . sValues

-- | Finalize all pending symbolic variable declarations.
finalize :: SMT.Solver -> Store -> IO Store
finalize solver store@(Store {sValues = m, defined = defs}) = do
  let new = m `Map.difference` defs
  mapM_ (uncurry declareSymbolic) $ Map.toList new

  pure
    store
      { defined = Map.union m new,
        sValues = Map.empty
      }
  where
    declareSymbolic n v =
      SMT.declareBV solver n $ SE.bitSize v

-- | Create a variable store from a 'Model'.
setModel :: Store -> Model.Model -> Store
setModel store model =
  store {cValues = Map.fromList $ Model.toList model}

-- | Lookup the variable name in the store, if it doesn't exist return
-- an unconstrained 'CE.Concolic' value with a random concrete part.
getConcolic :: Store -> String -> QBE.ExtType -> (Store, CE.Concolic DE.RegVal)
getConcolic store@Store {randGen = rand} name ty =
  ( store {sValues = newSymVars, randGen = nextRand},
    CE.Concolic concrete (Just symbolic)
  )
  where
    (symbolic, newSymVars) =
      let bv = SE.symbolic name ty
       in (bv, Map.insert name bv $ sValues store)

    (concrete, nextRand) =
      case Map.lookup name (cValues store) of
        Just cv -> (cv, rand)
        Nothing ->
          -- TODO: The generator should consider the bounds of QBE.BaseType.
          let (rv, nr) = genWord64 rand
              conValue = E.fromLit ty rv
           in (conValue, nr)
