-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Backend.Store
  ( Store,
    empty,
    toList,
    setModel,
    getConcolic,
  )
where

import Data.Functor (($>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Language.QBE.Backend (Model)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import System.Random.Stateful (IOGenM, StdGen, initStdGen, newIOGenM, uniformWord64)

-- A variable store mapping variable names to concrete values.
data Store
  = Store
  { cValues :: IORef (Map.Map String DE.RegVal),
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

toList :: Store -> IO [(String, DE.RegVal)]
toList store = Map.toList <$> readIORef (cValues store)

-- | Create a variable store from a 'Model'.
setModel :: Store -> Model -> IO ()
setModel store model =
  let modelMap = Map.fromList (map go model)
   in writeIORef (cValues store) modelMap
  where
    -- TODO: Better error handling → revise 'Model' type.
    go :: (SMT.SExpr, SMT.Value) -> (String, DE.RegVal)
    go (SMT.Atom name, SMT.Bits n v) =
      case DE.fromBits n v of
        Just x -> (name, x)
        Nothing -> error "invalid bitsize in solver model"
    go _ = error "malformed entry in solver model"

-- | Lookup the variable name in the store, if it doesn't exist return
-- an unconstrained 'CE.Concolic' value with a random concrete part.
getConcolic :: SMT.Solver -> Store -> String -> QBE.BaseType -> IO CE.Concolic
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
