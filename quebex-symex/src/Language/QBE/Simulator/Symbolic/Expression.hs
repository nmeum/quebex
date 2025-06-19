module Language.QBE.Simulator.Symbolic.Expression
  ( BitVector (..),
    half,
    toBytes,
    fromBytes,
  )
where

import Control.Exception (assert)
import Data.Word (Word16)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT

-- TODO: Consider making this a sum type
data BitVector
  = BitVector
  { sexpr :: SMT.SExpr,
    qtype :: QBE.ExtType
  }
  deriving (Show)

half :: Word16 -> BitVector
half v = BitVector (SMT.bvBin 16 $ fromIntegral v) QBE.HalfWord

bitSize :: QBE.ExtType -> Int
bitSize ty = QBE.extTypeByteSize ty * 8

------------------------------------------------------------------------

toBytes :: BitVector -> [BitVector]
toBytes BitVector {sexpr = s, qtype = ty} =
  assert (size `mod` 8 == 0) $
    map (\n -> BitVector (nthByte s n) QBE.Byte) [1 .. size `div` 8]
  where
    size :: Integer
    size = fromIntegral $ bitSize ty

    nthByte :: SMT.SExpr -> Integer -> SMT.SExpr
    nthByte expr n = SMT.extract expr ((n * 8) - 1) ((n - 1) * 8)

fromBytes :: QBE.ExtType -> [BitVector] -> Maybe BitVector
fromBytes _ [] = Nothing
fromBytes ty bytes@(BitVector {sexpr = s} : xs) =
  if (length bytes /= QBE.extTypeByteSize ty)
    then Nothing
    else Just $ BitVector (foldl concatBV s xs) ty
  where
    concatBV :: SMT.SExpr -> BitVector -> SMT.SExpr
    concatBV acc byte =
      assert (qtype byte == QBE.Byte) $
        SMT.concat (sexpr byte) acc

instance E.Storable BitVector BitVector where
  toBytes = toBytes
  fromBytes = fromBytes

------------------------------------------------------------------------

-- instance E.ValueRepr BitVector where
