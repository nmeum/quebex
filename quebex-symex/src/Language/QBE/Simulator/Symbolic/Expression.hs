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

-- This expression language has more refined internal types. This are
-- needed to emit better (i.e., less complex) SMT queries, leading to
-- improved solver time.
data SymType
  = SymBool
  | SymByte
  | SymWord
  | SymLong
  deriving (Show, Eq)

baseToSym :: QBE.BaseType -> SymType
baseToSym QBE.Word = SymWord
baseToSym QBE.Long = SymLong
baseToSym QBE.Single = error "symbolic single not supported"
baseToSym QBE.Double = error "symbolic double not supported"

symBitSize :: SymType -> Int
symBitSize SymBool = 1
symBitSize SymByte = 8
symBitSize SymWord = 32
symBitSize SymLong = 64

-- This types supports more than just the 'QBE.BaseType' to ease the
-- implementation of the 'MEM.Storable' type class. Essentially, this
-- data type just provides a generic BitVector abstraction on top of
-- 'SMT.SExpr'.
data BitVector
  = BitVector
  { sexpr :: SMT.SExpr,
    -- NOTE: We cannot rely on the 'SMT.Value' return via 'SMT.sexprToVal'
    -- for 'sexpr' as SimpleSMT largely relies on untyped S-Expressions for
    -- newly constructed expressions and cannot infer their return type.
    -- Therefore, we need our expressions constructors to track this separately.
    qtype :: SymType
  }
  deriving (Show, Eq)

fromByte :: Word8 -> BitVector
fromByte byte = BitVector (SMT.bvBin 8 $ fromIntegral byte) SymByte

fromReg :: D.RegVal -> BitVector
fromReg (D.VWord v) = BitVector (SMT.bvBin 32 $ fromIntegral v) SymWord
fromReg (D.VLong v) = BitVector (SMT.bvBin 64 $ fromIntegral v) SymLong
fromReg (D.VSingle _) = error "symbolic floats not supported"
fromReg (D.VDouble _) = error "symbolic doubles not supported"

fromSExpr :: QBE.BaseType -> SMT.SExpr -> BitVector
fromSExpr ty sexpr = BitVector sexpr (baseToSym ty)

toSExpr :: BitVector -> SMT.SExpr
toSExpr = sexpr

symbolic :: SMT.Solver -> String -> QBE.BaseType -> IO BitVector
symbolic solver name ty = do
  let bits = SMT.tBits $ fromIntegral (QBE.baseTypeBitSize ty)
  sym <- SMT.declare solver name bits
  return $ BitVector sym (baseToSym ty)

-- In the QBE a condition (see `jnz`) is true if the Word value is not zero.
toCond :: Bool -> BitVector -> SMT.SExpr
toCond isTrue BitVector {sexpr = s, qtype = ty} =
  -- Equality is only defined for Words.
  assert (ty == SymWord) $
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
      map (\n -> BitVector (nthByte s n) SymByte) [1 .. size `div` 8]
    where
      size :: Integer
      size = fromIntegral $ symBitSize ty

      nthByte :: SMT.SExpr -> Integer -> SMT.SExpr
      nthByte expr n = SMT.extract expr ((n * 8) - 1) ((n - 1) * 8)

  fromBytes _ [] = Nothing
  fromBytes ty bytes@(BitVector {sexpr = s} : xs) =
    if length bytes /= fromIntegral (QBE.loadByteSize ty)
      then Nothing
      else case (ty, bytes) of
        (QBE.LSubWord QBE.UnsignedByte, [_]) ->
          Just (BitVector (SMT.zeroExtend 24 concated) SymWord)
        (QBE.LSubWord QBE.SignedByte, [_]) ->
          Just (BitVector (SMT.signExtend 24 concated) SymWord)
        (QBE.LSubWord QBE.SignedHalf, [_, _]) ->
          Just (BitVector (SMT.signExtend 16 concated) SymWord)
        (QBE.LSubWord QBE.UnsignedHalf, [_, _]) ->
          Just (BitVector (SMT.zeroExtend 16 concated) SymWord)
        (QBE.LBase QBE.Word, [_, _, _, _]) ->
          Just (BitVector concated SymWord)
        (QBE.LBase QBE.Long, [_, _, _, _, _, _, _, _]) ->
          Just (BitVector concated SymLong)
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
        assert (qtype byte == SymByte) $
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
    let syty = baseToSym ty
        size = fromIntegral $ symBitSize syty
     in BitVector (SMT.bvBin size $ fromIntegral n) syty

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

  wordToLong (QBE.SLSubWord QBE.SignedByte) (BitVector {sexpr = s, qtype = SymWord}) =
    Just $ BitVector (SMT.signExtend 56 (SMT.extract s 7 0)) SymLong
  wordToLong (QBE.SLSubWord QBE.UnsignedByte) (BitVector {sexpr = s, qtype = SymWord}) =
    Just $ BitVector (SMT.zeroExtend 56 (SMT.extract s 7 0)) SymLong
  wordToLong (QBE.SLSubWord QBE.SignedHalf) (BitVector {sexpr = s, qtype = SymWord}) =
    Just $ BitVector (SMT.signExtend 48 (SMT.extract s 15 0)) SymLong
  wordToLong (QBE.SLSubWord QBE.UnsignedHalf) (BitVector {sexpr = s, qtype = SymWord}) =
    Just $ BitVector (SMT.zeroExtend 48 (SMT.extract s 15 0)) SymLong
  wordToLong QBE.SLSignedWord (BitVector {sexpr = s, qtype = SymWord}) =
    Just $ BitVector (SMT.signExtend 32 s) SymLong
  wordToLong QBE.SLUnsignedWord (BitVector {sexpr = s, qtype = SymWord}) =
    Just $ BitVector (SMT.zeroExtend 32 s) SymLong
  wordToLong _ _ = Nothing

  subType QBE.Word v@(BitVector {qtype = SymWord}) = Just v
  subType QBE.Word (BitVector {qtype = SymLong, sexpr = s}) =
    Just $ BitVector (SMT.extract s 31 0) SymWord
  subType QBE.Long v@(BitVector {qtype = SymLong}) = Just v
  subType _ _ = Nothing

  -- XXX: This only works for constants values, but this is fine (see above).
  isZero bv =
    case SMT.sexprToVal (sexpr bv) of
      SMT.Bits _ v -> v == 0
      _ -> error "unreachable"

  add = binaryOp SMT.bvAdd
  sub = binaryOp SMT.bvSub
  mul = binaryOp SMT.bvMul
  or = binaryOp SMT.bvOr
  xor = binaryOp SMT.bvXOr
  and = binaryOp SMT.bvAnd
  urem = binaryOp SMT.bvURem
  srem = binaryOp SMT.bvSRem
  udiv = binaryOp SMT.bvUDiv

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
