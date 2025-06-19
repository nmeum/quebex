module Language.QBE.Simulator.Concolic.Expression
  ( Concolic(concrete, symbolic),
    hasSymbolic
  )
where

import SimpleSMT qualified as SMT
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Default.Expression qualified as D

data Concolic
  = Concolic
  { concrete :: D.RegVal,
    symbolic :: Maybe SMT.SExpr
  }

hasSymbolic :: Concolic -> Bool
hasSymbolic Concolic { symbolic = Just _ } = True
hasSymbolic _ = False

-- TODO: Storable instance
