-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Backend.Store
  ( Store,
    Assign,
    empty,
    sexprs,
    assign,
    setModel,
    getConcolic,
  )
where

import Data.Functor (($>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Language.QBE.Backend.Model qualified as Model
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import System.Random.Stateful (IOGenM, StdGen, initStdGen, newIOGenM, uniformWord64)

-- | Concrete variable assignment.
type Assign = Map.Map String DE.RegVal

-- A variable store mapping variable names to concrete values.
data Store
  = Store
  { cValues :: IORef Assign,
    sValues :: IORef (Map.Map String SE.BitVector),
    randGen :: IOGenM StdGen
  }

-- | Create a new (empty) store.
empty :: IO Store
empty = do
  ranGen <- initStdGen >>= newIOGenM
  conMap <- newIORef Map.empty
  symMap <- newIORef Map.empty
  pure $ Store conMap symMap ranGen

-- | Obtain symbolic values as a list of 'SimpleSMT' expressions.
sexprs :: Store -> IO [SMT.SExpr]
sexprs Store {sValues = m} =
  map SE.toSExpr . Map.elems <$> readIORef m

-- | Obtain a list of concrete variable assignments.
assign :: Store -> IO Assign
assign store = readIORef (cValues store)

-- | Create a variable store from a 'Model'.
setModel :: Store -> Model.Model -> IO ()
setModel store model = do
  modelMap <- Map.fromList <$> Model.toList model
  writeIORef (cValues store) modelMap

-- | Lookup the variable name in the store, if it doesn't exist return
-- an unconstrained 'CE.Concolic' value with a random concrete part.
getConcolic :: SMT.Solver -> Store -> String -> QBE.BaseType -> IO (CE.Concolic DE.RegVal)
getConcolic solver store@Store {cValues = con, sValues = sym} name ty = do
  concreteMap <- readIORef con
  concrete <-
    case Map.lookup name concreteMap of
      Just x -> pure x
      Nothing ->
        -- TODO: The uniform generator must consider the bounds of QBE.BaseType.
        E.fromLit ty <$> uniformWord64 (randGen store)

  symbolicMap <- readIORef sym
  symbolic <-
    case Map.lookup name symbolicMap of
      Just x -> pure x
      Nothing -> do
        s <- SE.symbolic solver name ty
        writeIORef sym (Map.insert name s symbolicMap) $> s

  pure $ CE.Concolic concrete (Just symbolic)
