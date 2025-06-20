module Language.QBE.Simulator.Symbolic where

import Language.QBE.Types qualified as QBE

bitSize :: QBE.ExtType -> Int
bitSize ty = QBE.extTypeByteSize ty * 8
