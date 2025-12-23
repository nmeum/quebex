-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Symbolic.Expression
  ( BitVector,
    fromByte,
    fromReg,
    toSExpr,
    symbolic,
    bitSize,
    toCond,
  )
where

import Control.Exception (assert)
import Data.Bits (shiftL, (.&.))
import Data.Word (Word64, Word8)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE
import SimpleBV qualified as SMT

-- TODO: Floating point support.
newtype BitVector = BitVector SMT.SExpr
  deriving (Show, Eq)

fromByte :: Word8 -> BitVector
fromByte byte = BitVector (SMT.bvLit 8 $ fromIntegral byte)

fromReg :: D.RegVal -> BitVector
fromReg (D.VByte v) = BitVector (SMT.bvLit 8 $ fromIntegral v)
fromReg (D.VHalf v) = BitVector (SMT.bvLit 16 $ fromIntegral v)
fromReg (D.VWord v) = BitVector (SMT.bvLit 32 $ fromIntegral v)
fromReg (D.VLong v) = BitVector (SMT.bvLit 64 $ fromIntegral v)
fromReg (D.VSingle _) = error "symbolic floats not supported"
fromReg (D.VDouble _) = error "symbolic doubles not supported"

toSExpr :: BitVector -> SMT.SExpr
toSExpr (BitVector s) = s

symbolic :: String -> QBE.ExtType -> BitVector
symbolic name ty = BitVector (SMT.const name $ QBE.extTypeBitSize ty)

bitSize :: BitVector -> Int
bitSize = SMT.width . toSExpr

-- In the QBE a condition (see `jnz`) is true if the Word value is not zero.
toCond :: Bool -> BitVector -> SMT.SExpr
toCond isTrue bv =
  -- Equality is only defined for Words.
  assert (bitSize bv == QBE.baseTypeBitSize QBE.Word) $
    let zeroSExpr = toSExpr (fromReg $ E.fromLit (QBE.Base QBE.Word) 0)
     in toCond' (toSExpr bv) zeroSExpr
  where
    toCond' lhs rhs
      | isTrue = SMT.not (SMT.eq lhs rhs) -- /= 0
      | otherwise = SMT.eq lhs rhs -- == 0

------------------------------------------------------------------------

instance MEM.Storable BitVector BitVector where
  toBytes (BitVector s) =
    assert (size `mod` 8 == 0) $
      map (BitVector . nthByte s) [1 .. fromIntegral size `div` 8]
    where
      size :: Integer
      size = fromIntegral $ SMT.width s

      nthByte :: SMT.SExpr -> Int -> SMT.SExpr
      nthByte expr n = SMT.extract expr ((n - 1) * 8) 8

  fromBytes _ [] = Nothing
  fromBytes ty bytes@(BitVector s : xs) =
    if length bytes /= fromIntegral (QBE.loadByteSize ty)
      then Nothing
      else case (ty, bytes) of
        (QBE.LSubWord QBE.UnsignedByte, [_]) ->
          Just (BitVector (SMT.zeroExtend 24 concated))
        (QBE.LSubWord QBE.SignedByte, [_]) ->
          Just (BitVector (SMT.signExtend 24 concated))
        (QBE.LSubWord QBE.SignedHalf, [_, _]) ->
          Just (BitVector (SMT.signExtend 16 concated))
        (QBE.LSubWord QBE.UnsignedHalf, [_, _]) ->
          Just (BitVector (SMT.zeroExtend 16 concated))
        (QBE.LBase QBE.Word, [_, _, _, _]) ->
          Just (BitVector concated)
        (QBE.LBase QBE.Long, [_, _, _, _, _, _, _, _]) ->
          Just (BitVector concated)
        (QBE.LBase QBE.Single, [_, _, _, _]) ->
          error "float loading not implemented"
        (QBE.LBase QBE.Double, [_, _, _, _, _, _, _, _]) ->
          error "double loading not implemented"
        _ -> Nothing
    where
      concated :: SMT.SExpr
      concated = foldl concatBV s xs

      concatBV :: SMT.SExpr -> BitVector -> SMT.SExpr
      concatBV acc (BitVector byte) =
        assert (SMT.width byte == 8) $
          SMT.concat byte acc

------------------------------------------------------------------------

binaryOp :: (SMT.SExpr -> SMT.SExpr -> SMT.SExpr) -> BitVector -> BitVector -> Maybe BitVector
binaryOp op (BitVector lhs) (BitVector rhs)
  | SMT.width lhs == SMT.width rhs = Just $ BitVector (lhs `op` rhs)
  | otherwise = Nothing

-- TODO: Move this into the expression abstraction.
toShiftAmount :: Word64 -> BitVector -> Maybe BitVector
toShiftAmount size amount = amount `E.urem` E.fromLit (QBE.Base QBE.Word) size

shiftOp :: (SMT.SExpr -> SMT.SExpr -> SMT.SExpr) -> BitVector -> BitVector -> Maybe BitVector
shiftOp op value amount@(BitVector SMT.Word) =
  case bitSize value of
    32 -> toShiftAmount 32 amount >>= binaryOp op value
    64 -> do
      shiftAmount <- toShiftAmount 64 amount
      E.wordToLong QBE.SLUnsignedWord shiftAmount >>= binaryOp op value
    _ -> Nothing
shiftOp _ _ _ = Nothing -- Shift amount must always be a Word.

binaryBoolOp :: (SMT.SExpr -> SMT.SExpr -> SMT.SExpr) -> BitVector -> BitVector -> Maybe BitVector
binaryBoolOp op lhs rhs = do
  bv <- binaryOp op lhs rhs
  return $ BitVector (SMT.ite (toSExpr bv) trueValue falseValue)
  where
    -- TODO: Declare these as constants.
    trueValue :: SMT.SExpr
    trueValue = toSExpr $ E.fromLit (QBE.Base QBE.Long) 1

    falseValue :: SMT.SExpr
    falseValue = toSExpr $ E.fromLit (QBE.Base QBE.Long) 0

instance E.ValueRepr BitVector where
  fromLit ty n =
    let size = QBE.extTypeBitSize ty
        mask = (1 `shiftL` size) - 1
     in BitVector $ SMT.bvLit (fromIntegral size) $ fromIntegral (n .&. mask)

  fromFloat = error "symbolic floats currently unsupported"
  fromDouble = error "symbolic doubles currently unsupported"

  -- XXX: This only works for constants values, but this is fine since we implement
  -- concolic execution and can obtain the address from the concrete value part.
  toWord64 (BitVector value) =
    case SMT.sexprToVal value of
      SMT.Bits _ n -> fromIntegral n
      _ -> error "unrechable"

  wordToLong (QBE.SLSubWord QBE.SignedByte) (BitVector s@SMT.Word) =
    Just $ BitVector (SMT.signExtend 56 (SMT.extract s 0 8))
  wordToLong (QBE.SLSubWord QBE.UnsignedByte) (BitVector s@SMT.Word) =
    Just $ BitVector (SMT.zeroExtend 56 (SMT.extract s 0 8))
  wordToLong (QBE.SLSubWord QBE.SignedHalf) (BitVector s@SMT.Word) =
    Just $ BitVector (SMT.signExtend 48 (SMT.extract s 0 16))
  wordToLong (QBE.SLSubWord QBE.UnsignedHalf) (BitVector s@SMT.Word) =
    Just $ BitVector (SMT.zeroExtend 48 (SMT.extract s 0 16))
  wordToLong QBE.SLSignedWord (BitVector s@SMT.Word) =
    Just $ BitVector (SMT.signExtend 32 s)
  wordToLong QBE.SLUnsignedWord (BitVector s@SMT.Word) =
    Just $ BitVector (SMT.zeroExtend 32 s)
  wordToLong _ _ = Nothing

  subType QBE.Word v@(BitVector SMT.Word) = Just v
  subType QBE.Word (BitVector s@SMT.Long) =
    Just $ BitVector (SMT.extract s 0 32)
  subType QBE.Long v@(BitVector SMT.Long) = Just v
  subType _ _ = Nothing

  add = binaryOp SMT.bvAdd
  sub = binaryOp SMT.bvSub
  mul = binaryOp SMT.bvMul
  div = binaryOp SMT.bvSDiv
  or = binaryOp SMT.bvOr
  xor = binaryOp SMT.bvXOr
  and = binaryOp SMT.bvAnd
  urem = binaryOp SMT.bvURem
  srem = binaryOp SMT.bvSRem
  udiv = binaryOp SMT.bvUDiv

  neg (BitVector v) = Just $ BitVector (SMT.bvNeg v)

  sar = shiftOp SMT.bvAShr
  shr = shiftOp SMT.bvLShr
  shl = shiftOp SMT.bvShl

  eq = binaryBoolOp SMT.eq
  ne = binaryBoolOp (\lhs rhs -> SMT.not $ SMT.eq lhs rhs)
  sle = binaryBoolOp SMT.bvSLeq
  slt = binaryBoolOp SMT.bvSLt
  sge = binaryBoolOp (flip SMT.bvSLeq)
  sgt = binaryBoolOp (flip SMT.bvSLt)
  ule = binaryBoolOp SMT.bvULeq
  ult = binaryBoolOp SMT.bvULt
  uge = binaryBoolOp (\lhs rhs -> SMT.or (SMT.bvULt rhs lhs) (SMT.eq lhs rhs))
  ugt = binaryBoolOp (flip SMT.bvULt)
