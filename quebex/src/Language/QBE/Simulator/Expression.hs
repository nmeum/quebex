module Language.QBE.Simulator.Expression where

import Data.Word (Word64)
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE

class Storable v b where
  toBytes :: v -> [b]
  fromBytes :: QBE.ExtType -> [b] -> Maybe v

class ValueRepr v where
  -- TODO: Change this to fromWord64 and rely on long subtyping
  fromLit :: QBE.BaseType -> Word64 -> v
  fromFloat :: Float -> v
  fromDouble :: Double -> v

  fromAddress :: MEM.Address -> v
  toAddress :: v -> Maybe MEM.Address

  extend :: QBE.SubWordType -> v -> Maybe v
  subType :: QBE.BaseType -> v -> Maybe v
  isZero :: v -> Bool

  add :: v -> v -> Maybe v
  sub :: v -> v -> Maybe v
