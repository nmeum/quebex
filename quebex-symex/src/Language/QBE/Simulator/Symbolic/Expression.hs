module Language.QBE.Simulator.Symbolic.Expression
  ( BitVector,
    getValue,
  )
where

import Control.Exception (assert)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT

-- TODO: Consider making this a sum type
data BitVector
  = BitVector
  { sexpr :: SMT.SExpr,
    qtype :: QBE.ExtType
  }
  deriving (Show, Eq)

-- | Only intended for testing purposes.
getValue :: BitVector -> SMT.SExpr
getValue = sexpr

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

binaryOp :: (SMT.SExpr -> SMT.SExpr -> SMT.SExpr) -> BitVector -> BitVector -> Maybe BitVector
binaryOp op lhs@(BitVector {sexpr = slhs}) rhs@(BitVector {sexpr = srhs})
  | qtype lhs == qtype rhs = Just $ lhs {sexpr = slhs `op` srhs}
  | otherwise = Nothing

-- TODO: If we Change E.ValueRepr to operate in 'Exec' then we can do IO stuff here.
instance E.ValueRepr BitVector where
  fromLit ty n =
    let exty = QBE.Base ty
        size = fromIntegral $ bitSize exty
     in BitVector (SMT.bvBin size $ fromIntegral n) exty

  fromFloat = error "symbolic floats currently unsupported"
  fromDouble = error "symbolic doubles currently unsupported"

  fromAddress addr =
    let ty = QBE.Base QBE.Long
     in BitVector (SMT.bvBin (fromIntegral (bitSize ty)) (fromIntegral addr)) ty

  -- XXX: This only works for constants values, but this is fine since we implement
  -- concolic execution and can obtain the address from the concrete value part.
  toAddress addr =
    case SMT.sexprToVal (sexpr addr) of
      -- TODO: Don't hardcode address type
      SMT.Bits 64 n -> Just $ fromIntegral n
      _ -> Nothing

  -- TODO: Don't hardcode bitsizes here
  -- TODO: refactor (just need to select sign/zero extend)
  extend QBE.SignedByte (BitVector {sexpr = s, qtype = QBE.Byte}) =
    Just $ BitVector (SMT.signExtend 56 s) (QBE.Base QBE.Long)
  extend QBE.UnsignedByte (BitVector {sexpr = s, qtype = QBE.Byte}) =
    Just $ BitVector (SMT.zeroExtend 56 s) (QBE.Base QBE.Long)
  extend QBE.SignedHalf (BitVector {sexpr = s, qtype = QBE.HalfWord}) =
    Just $ BitVector (SMT.signExtend 48 s) (QBE.Base QBE.Long)
  extend QBE.UnsignedHalf (BitVector {sexpr = s, qtype = QBE.HalfWord}) =
    Just $ BitVector (SMT.zeroExtend 48 s) (QBE.Base QBE.Long)
  extend _ _ = Nothing

  subType QBE.Word v@(BitVector {qtype = QBE.Base QBE.Word}) = Just v
  subType QBE.Word (BitVector {qtype = QBE.Base QBE.Long, sexpr = s}) =
    Just $ BitVector (SMT.extract s 31 0) (QBE.Base QBE.Word)
  subType QBE.Long v@(BitVector {qtype = QBE.Base QBE.Long}) = Just v
  subType QBE.Single v@(BitVector {qtype = QBE.Base QBE.Single}) = Just v
  subType QBE.Double v@(BitVector {qtype = QBE.Base QBE.Double}) = Just v
  subType _ _ = Nothing

  -- XXX: This only works for constants values, but this is fine (see above).
  isZero bv =
    case SMT.sexprToVal (sexpr bv) of
      SMT.Bits _ v -> v == 0
      _ -> error "unreachable"

  add = binaryOp SMT.bvAdd
  sub = binaryOp SMT.bvSub
