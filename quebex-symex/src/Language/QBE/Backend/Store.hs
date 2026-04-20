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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, StateT, get, modify, put)
import Data.Map qualified as Map
import Language.QBE.Backend.Model qualified as Model
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleBV qualified as SMT
import System.Random (StdGen, genWord64R)

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
empty :: StdGen -> Store
empty = Store Map.empty Map.empty Map.empty

-- | Obtain symbolic values as a list of 'SimpleBV' expressions.
sexprs :: Store -> [SMT.SExpr]
sexprs = map SE.toSExpr . Map.elems . sValues

------------------------------------------------------------------------

-- | Finalize all pending symbolic variable declarations.
-- This is the only function that modifies the solver state.
finalize :: SMT.Solver -> StateT Store IO ()
finalize solver = do
  store@(Store {sValues = vals, defined = defs}) <- get

  let new = vals `Map.difference` defs
  mapM_ (liftIO . uncurry declareSymbolic) $ Map.toList new

  put $
    store
      { defined = Map.union vals new,
        sValues = Map.empty
      }
  where
    declareSymbolic n v =
      SMT.declareBV solver n $ SE.bitSize v

-- | Create a variable store from a 'Model'.
setModel :: Model.Model -> State Store ()
setModel model = do
  let vals = Map.fromList $ Model.toList model
  modify (\s -> s {cValues = vals})

-- | Generate a random 'DE.RegVal' for a value of a given 'QBE.ExtType'.
genRandom :: String -> QBE.ExtType -> State Store DE.RegVal
genRandom name ty = do
  store <- get
  let (cv, nr) = randomExtType (randGen store) ty
      newCVals = Map.insert name cv $ cValues store
  put $ store {cValues = newCVals, randGen = nr}
  pure cv
  where
    randomExtType rg extTy =
      let maxValue = (2 ^ QBE.extTypeBitSize ty) - 1
          (rv, nr) = genWord64R maxValue rg
       in (E.fromLit extTy rv, nr)

-- | Lookup the variable name in the store, if it doesn't exist return
-- an unconstrained 'CE.Concolic' value with a random concrete part.
getConcolic :: String -> QBE.ExtType -> State Store (CE.Concolic DE.RegVal)
getConcolic name ty = do
  store <- get

  concrete <-
    case Map.lookup name (cValues store) of
      Just cv -> pure cv
      Nothing -> genRandom name ty

  let symbolic = SE.symbolic name ty
      newSVals = Map.insert name symbolic $ sValues store
  modify (\s -> s {sValues = newSVals})

  pure $ CE.Concolic concrete (Just symbolic)
