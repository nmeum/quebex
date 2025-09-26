-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
module Language.QBE.Backend.Model
  ( Model,
    toList,
    getModel,
  )
where

import Language.QBE.Simulator.Default.Expression qualified as DE
import SimpleBV qualified as SMT

-- Assignments returned by the Solver for a given query.
newtype Model = Model [(String, SMT.Value)]
  deriving (Show, Eq)

-- | Get a new 'Model' for a list of input variables that should be contained in it.
getModel :: SMT.Solver -> [SMT.SExpr] -> IO Model
getModel solver inputVars = Model <$> SMT.getValues solver inputVars

-- | Convert a model to a list of concrete variable assignments.
toList :: Model -> [(String, DE.RegVal)]
toList (Model lst) = map go lst
  where
    go :: (String, SMT.Value) -> (String, DE.RegVal)
    go (name, SMT.Bits n v) =
      case DE.fromBits n v of
        Just x -> (name, x)
        Nothing -> error "invalid bitvector size"
    go _ = error "unsupported value type"
