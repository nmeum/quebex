module Language.QBE.Simulator.Symbolic.Store
  ( Store,
    empty,
    setModel,
    getConcolic,
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Map qualified as Map
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Simulator.Symbolic.Tracer (Model)
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import System.Random.Stateful (IOGenM, StdGen, initStdGen, newIOGenM, uniformWord64)

-- A variable store mapping variable names to concrete values.
data Store
  = Store
  { cValues :: Map.Map String DE.RegVal,
    sValues :: IORef (Map.Map String SE.BitVector),
    randGen :: IOGenM StdGen
  }

instance Show Store where
  show (Store {cValues = m}) =
    intercalate "\n" $
      map (\(k, v) -> k ++ "\t= " ++ show v) (Map.toList m)

-- | Create a new (empty) store.
empty :: IO Store
empty = do
  r <- initStdGen
  g <- newIOGenM r
  s <- newIORef Map.empty
  pure $ Store Map.empty s g

-- | Create a variable store from a 'Model'.
setModel :: Store -> Model -> Maybe Store
setModel store model = do
  modelMap <- Map.fromList <$> mapM go model
  pure $ store {cValues = modelMap}
  where
    go :: (SMT.SExpr, SMT.Value) -> Maybe (String, DE.RegVal)
    go (SMT.Atom name, SMT.Bits n v) = do
      regVal <- DE.fromBits n v
      pure (name, regVal)
    go _ = Nothing

-- | Lookup the variable name in the store, if it doesn't exist return
-- an unconstrained 'CE.Concolic' value with a random concrete part.
getConcolic :: SMT.Solver -> Store -> String -> QBE.BaseType -> IO CE.Concolic
getConcolic solver store@Store {sValues = sym} name ty = do
  concrete <-
    case Map.lookup name (cValues store) of
      Just x -> pure x
      Nothing ->
        -- TODO: The uniform generater must consider the bounds of QBE.BaseType.
        E.fromLit ty <$> uniformWord64 (randGen store)

  symbolicMap <- readIORef sym
  symbolic <-
    case Map.lookup name symbolicMap of
      Just x -> pure x
      Nothing -> do
        symbolic <- SE.symbolic solver name ty
        writeIORef sym (Map.insert name symbolic symbolicMap)
        pure symbolic

  pure $ CE.Concolic concrete (Just symbolic)
