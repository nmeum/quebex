-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Symbolic.Expression
  ( BitVector,
    fromByte,
    fromReg,
    fromSExpr,
    toSExpr,
    symbolic,
    toCond,
  )
where

import Control.Exception (assert)
import Data.Word (Word8)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT

-- This types supports more than just the 'QBE.BaseType' becasue …
data BitVector
  = BitVector
  { sexpr :: SMT.SExpr,
    -- TODO: For BitVectors, we can extract the type from the SExpr.
    -- However, in the long run we also want to support Single/Double.
    -- Though we could do a case distinction over 'SMT.sexprToVal' in
    -- order to identify these.
    qtype :: QBE.ExtType
  }
  deriving (Show, Eq)

fromByte :: Word8 -> BitVector
fromByte byte = BitVector (SMT.bvBin 8 $ fromIntegral byte) QBE.Byte

fromReg :: D.RegVal -> BitVector
fromReg (D.VWord v) = BitVector (SMT.bvBin 32 $ fromIntegral v) (QBE.Base QBE.Word)
fromReg (D.VLong v) = BitVector (SMT.bvBin 64 $ fromIntegral v) (QBE.Base QBE.Long)
fromReg (D.VSingle _) = error "symbolic floats not supported"
fromReg (D.VDouble _) = error "symbolic doubles not supported"

fromSExpr :: QBE.BaseType -> SMT.SExpr -> BitVector
fromSExpr ty sexpr = BitVector sexpr (QBE.Base ty)

toSExpr :: BitVector -> SMT.SExpr
toSExpr = sexpr

symbolic :: SMT.Solver -> String -> QBE.BaseType -> IO BitVector
symbolic solver name ty = do
  let bits = SMT.tBits $ fromIntegral (QBE.baseTypeBitSize ty)
  sym <- SMT.declare solver name bits
  return $ BitVector sym (QBE.Base ty)

-- In the QBE a condition (see `jnz`) is true if the Word value is not zero.
toCond :: Bool -> BitVector -> SMT.SExpr
toCond isTrue BitVector {sexpr = s, qtype = ty} =
  -- Equality is only defined for Words.
  assert (ty == QBE.Base QBE.Word) $
    let zeroSExpr = sexpr (fromReg $ E.fromLit QBE.Word 0)
     in toCond' s zeroSExpr
  where
    toCond' lhs rhs
      | isTrue = SMT.not (SMT.eq lhs rhs) -- /= 0
      | otherwise = SMT.eq lhs rhs -- == 0

------------------------------------------------------------------------

instance MEM.Storable BitVector BitVector where
  toBytes BitVector {sexpr = s, qtype = ty} =
    assert (size `mod` 8 == 0) $
      map (\n -> BitVector (nthByte s n) QBE.Byte) [1 .. size `div` 8]
    where
      size :: Integer
      size = fromIntegral $ QBE.extTypeBitSize ty

      nthByte :: SMT.SExpr -> Integer -> SMT.SExpr
      nthByte expr n = SMT.extract expr ((n * 8) - 1) ((n - 1) * 8)

  fromBytes _ [] = Nothing
  fromBytes ty bytes@(BitVector {sexpr = s} : xs) =
    if length bytes /= fromIntegral (QBE.loadByteSize ty)
      then Nothing
      else case (ty, bytes) of
        (QBE.LSubWord QBE.UnsignedByte, [_]) ->
          Just (BitVector (SMT.zeroExtend 24 concated) (QBE.Base QBE.Word))
        (QBE.LSubWord QBE.SignedByte, [_]) ->
          Just (BitVector (SMT.signExtend 24 concated) (QBE.Base QBE.Word))
        (QBE.LSubWord QBE.SignedHalf, [_, _]) ->
          Just (BitVector (SMT.signExtend 16 concated) (QBE.Base QBE.Word))
        (QBE.LSubWord QBE.UnsignedHalf, [_, _]) ->
          Just (BitVector (SMT.zeroExtend 16 concated) (QBE.Base QBE.Word))
        (QBE.LBase QBE.Word, [_, _, _, _]) ->
          Just (BitVector concated (QBE.Base QBE.Word))
        (QBE.LBase QBE.Long, [_, _, _, _, _, _, _, _]) ->
          Just (BitVector concated (QBE.Base QBE.Long))
        (QBE.LBase QBE.Single, [_, _, _, _]) ->
          error "float loading not implemented"
        (QBE.LBase QBE.Double, [_, _, _, _, _, _, _, _]) ->
          error "double loading not implemented"
        _ -> Nothing
    where
      concated :: SMT.SExpr
      concated = foldl concatBV s xs

      concatBV :: SMT.SExpr -> BitVector -> SMT.SExpr
      concatBV acc byte =
        assert (qtype byte == QBE.Byte) $
          SMT.concat (sexpr byte) acc

------------------------------------------------------------------------

binaryOp :: (SMT.SExpr -> SMT.SExpr -> SMT.SExpr) -> BitVector -> BitVector -> Maybe BitVector
binaryOp op lhs@(BitVector {sexpr = slhs}) rhs@(BitVector {sexpr = srhs})
  | qtype lhs == qtype rhs = Just $ lhs {sexpr = slhs `op` srhs}
  | otherwise = Nothing

binaryBoolOp :: (SMT.SExpr -> SMT.SExpr -> SMT.SExpr) -> BitVector -> BitVector -> Maybe BitVector
binaryBoolOp op lhs rhs = do
  bv <- binaryOp op lhs rhs
  -- TODO: Can we get rid of the ITE somehow?
  return $ fromSExpr QBE.Long (SMT.ite (toSExpr bv) trueValue falseValue)
  where
    -- TODO: Declare these as constants.
    trueValue :: SMT.SExpr
    trueValue = toSExpr $ E.fromLit QBE.Long 1

    falseValue :: SMT.SExpr
    falseValue = toSExpr $ E.fromLit QBE.Long 0

-- TODO: If we Change E.ValueRepr to operate in 'Exec' then we can do IO stuff here.
instance E.ValueRepr BitVector where
  fromLit ty n =
    let exty = QBE.Base ty
        size = fromIntegral $ QBE.extTypeBitSize exty
     in BitVector (SMT.bvBin size $ fromIntegral n) exty

  fromFloat = error "symbolic floats currently unsupported"
  fromDouble = error "symbolic doubles currently unsupported"

  -- XXX: This only works for constants values, but this is fine since we implement
  -- concolic execution and can obtain the address from the concrete value part.
  toAddress addr =
    case SMT.sexprToVal (sexpr addr) of
      -- TODO: Don't hardcode address type
      SMT.Bits 64 n -> Just $ fromIntegral n
      _ -> Nothing
  fromAddress addr = fromReg (E.fromLit QBE.Long addr)

  wordToLong (QBE.SLSubWord QBE.SignedByte) (BitVector {sexpr = s, qtype = QBE.Base QBE.Word}) =
    Just $ BitVector (SMT.signExtend 56 (SMT.extract s 7 0)) (QBE.Base QBE.Long)
  wordToLong (QBE.SLSubWord QBE.UnsignedByte) (BitVector {sexpr = s, qtype = QBE.Base QBE.Word}) =
    Just $ BitVector (SMT.zeroExtend 56 (SMT.extract s 7 0)) (QBE.Base QBE.Long)
  wordToLong (QBE.SLSubWord QBE.SignedHalf) (BitVector {sexpr = s, qtype = QBE.Base QBE.Word}) =
    Just $ BitVector (SMT.signExtend 48 (SMT.extract s 15 0)) (QBE.Base QBE.Long)
  wordToLong (QBE.SLSubWord QBE.UnsignedHalf) (BitVector {sexpr = s, qtype = QBE.Base QBE.Word}) =
    Just $ BitVector (SMT.zeroExtend 48 (SMT.extract s 15 0)) (QBE.Base QBE.Long)
  wordToLong QBE.SLSignedWord (BitVector {sexpr = s, qtype = QBE.Base QBE.Word}) =
    Just $ BitVector (SMT.signExtend 32 s) (QBE.Base QBE.Long)
  wordToLong QBE.SLUnsignedWord (BitVector {sexpr = s, qtype = QBE.Base QBE.Word}) =
    Just $ BitVector (SMT.zeroExtend 32 s) (QBE.Base QBE.Long)
  wordToLong _ _ = Nothing

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
  mul = binaryOp SMT.bvMul

  eq = binaryBoolOp SMT.eq
  ne = binaryBoolOp (\lhs rhs -> SMT.not $ SMT.eq lhs rhs)
  sle = binaryBoolOp SMT.bvSLeq
  slt = binaryBoolOp SMT.bvSLt
  sge = binaryBoolOp (\lhs rhs -> SMT.not $ SMT.bvSLt lhs rhs)
  sgt = binaryBoolOp (\lhs rhs -> SMT.not $ SMT.bvSLeq lhs rhs)
  ule = binaryBoolOp SMT.bvULeq
  ult = binaryBoolOp SMT.bvULt
  uge = binaryBoolOp (\lhs rhs -> SMT.not $ SMT.bvULt lhs rhs)
  ugt = binaryBoolOp (\lhs rhs -> SMT.not $ SMT.bvULeq lhs rhs)
