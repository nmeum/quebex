module Language.QBE.Simulator.Symbolic.Store
  ( Store,
    empty,
    setModel,
    getConcolic,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intercalate)
import Data.Map qualified as Map
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Simulator.Symbolic.Tracer (Model)
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
import System.Random.Stateful (IOGenM, StdGen, initStdGen, newIOGenM, uniformWord64)

-- A variable store mapping variable names to concrete values.
data Store
  = Store
  { sValues :: Map.Map String D.RegVal,
    -- TODO: Consider using an 'AtomicGenM' to enable concurrency.
    sRandGen :: IOGenM StdGen
  }

instance Show Store where
  show (Store {sValues = m}) =
    intercalate "\n" $
      map (\(k, v) -> k ++ "\t= " ++ show v) (Map.toList m)

-- | Create a new (empty) store.
empty :: (MonadIO m) => m Store
empty = do
  r <- initStdGen
  g <- newIOGenM r
  pure $ Store Map.empty g

-- | Create a variable store from a 'Model'.
setModel :: Store -> Model -> Maybe Store
setModel store model = do
  modelMap <- Map.fromList <$> mapM go model
  pure $ store {sValues = modelMap}
  where
    go :: (SMT.SExpr, SMT.Value) -> Maybe (String, D.RegVal)
    go (SMT.Atom name, SMT.Bits n v) = do
      regVal <- D.fromBits n v
      pure (name, regVal)
    go _ = Nothing

-- | Lookup the variable name in the store, if it doesn't exist return
-- an unconstrained 'CE.Concolic' value with a random concrete part.
getConcolic :: (MonadIO m) => SMT.Solver -> Store -> String -> QBE.BaseType -> m CE.Concolic
getConcolic solver Store {sValues = vals, sRandGen = randGen} name ty = do
  concrete <-
    case Map.lookup name vals of
      Just x -> pure x
      Nothing ->
        -- TODO: The uniform generater must consider the bounds of QBE.BaseType.
        E.fromLit ty <$> uniformWord64 randGen

  symbolic <- liftIO $ SE.symbolic solver name ty
  pure $ CE.Concolic concrete (Just symbolic)
