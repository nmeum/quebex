-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
module Language.QBE.Backend.Model
  ( Model,
    Error (..),
    toList,
    getModel,
  )
where

import Control.Exception (Exception, throwIO)
import Language.QBE.Simulator.Default.Expression qualified as DE
import SimpleSMT qualified as SMT

data Error
  = NonAtomNameSExpr
  | UnsupportedValueType
  | InvalidBitVectorSize
  deriving (Show)

instance Exception Error

------------------------------------------------------------------------

-- Assignments returned by the Solver for a given query.
newtype Model = Model [(String, SMT.Value)]
  deriving (Show, Eq)

-- | Get a new 'Model' for a list of input variables that should be contained in it.
getModel :: SMT.Solver -> [SMT.SExpr] -> IO Model
getModel solver inputVars = do
  lst <- SMT.getExprs solver inputVars >>= mapM go
  pure $ Model lst
  where
    go :: (SMT.SExpr, SMT.Value) -> IO (String, SMT.Value)
    go (SMT.Atom name, value) = pure (name, value)
    go _ = throwIO NonAtomNameSExpr

-- | Convert a model to a list of concrete variable assignments.
toList :: Model -> IO [(String, DE.RegVal)]
toList (Model lst) = mapM go lst
  where
    go :: (String, SMT.Value) -> IO (String, DE.RegVal)
    go (name, SMT.Bits n v) =
      case DE.fromBits n v of
        Just x -> pure (name, x)
        Nothing -> throwIO InvalidBitVectorSize
    go _ = throwIO UnsupportedValueType
