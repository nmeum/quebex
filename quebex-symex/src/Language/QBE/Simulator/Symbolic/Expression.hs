module Language.QBE.Simulator.Symbolic.Expression
  ( BitVector (..),
    half,
    toBytes,
  )
where

import Control.Exception (assert)
import Data.Word (Word16)
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT

data BitVector
  = BitVector
  { sexpr :: SMT.SExpr,
    qtype :: QBE.ExtType
  }
  deriving (Show)

half :: Word16 -> BitVector
half v = BitVector (SMT.bvBin 16 $ fromIntegral v) QBE.HalfWord

bitSize :: QBE.ExtType -> Integer
bitSize ty = QBE.extTypeByteSize ty * 8

------------------------------------------------------------------------

toBytes :: BitVector -> [BitVector]
toBytes BitVector {sexpr = s, qtype = ty} =
  assert (bitSize ty `mod` 8 == 0) $
    map (\n -> BitVector (nthByte s n) QBE.Byte) [1 .. bitSize ty `div` 8]
  where
    nthByte :: SMT.SExpr -> Integer -> SMT.SExpr
    nthByte expr n = SMT.extract expr ((n * 8) - 1) ((n - 1) * 8)
