-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Expression where

import Data.Word (Word64)
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE

-- TODO: We could use 'FlexibleContexts` here (which is part of GHC 2021)
-- to require a second type parameter which corresponds to the byte representation.
-- For example, 'Word8' for 'RegVal'. However, QBE requires an internal dynamic
-- typing representation anyhow so every 'ValueRepr' needs some way to represent
-- byte values anyhow. Nonetheless, a Word8 representation would allow using an
-- IOUArray instead of an IOARray for the Memory.
class Storable v where
  toBytes :: v -> [v]
  fromBytes :: QBE.ExtType -> [v] -> Maybe v

class ValueRepr v where
  -- TODO: Change this to fromWord64 and rely on long subtyping
  fromLit :: QBE.BaseType -> Word64 -> v
  fromFloat :: Float -> v
  fromDouble :: Double -> v

  fromAddress :: MEM.Address -> v
  toAddress :: v -> Maybe MEM.Address

  -- TODO: rename extend to extend to extendToLong
  extend :: QBE.SubWordType -> v -> Maybe v
  subType :: QBE.BaseType -> v -> Maybe v
  isZero :: v -> Bool

  add :: v -> v -> Maybe v
  sub :: v -> v -> Maybe v
  mul :: v -> v -> Maybe v
