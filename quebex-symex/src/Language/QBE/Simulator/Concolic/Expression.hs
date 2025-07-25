-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Concolic.Expression
  ( Concolic (..),
    hasSymbolic,
  )
where

import Control.Exception (assert)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.Symbolic.Expression qualified as SE

data Concolic v
  = Concolic
  { concrete :: v,
    symbolic :: Maybe SE.BitVector
  }
  deriving (Show)

hasSymbolic :: Concolic v -> Bool
hasSymbolic Concolic {symbolic = Just _} = True
hasSymbolic _ = False

getSymbolicDef :: (v -> SE.BitVector) -> Concolic v -> SE.BitVector
getSymbolicDef conc Concolic {concrete = c, symbolic = s} =
  fromMaybe (conc c) s

------------------------------------------------------------------------

instance MEM.Storable (Concolic D.RegVal) (Concolic Word8) where
  toBytes Concolic {concrete = c, symbolic = s} =
    let cbytes = MEM.toBytes c
        nbytes = length cbytes
        sbytes = maybe (replicate nbytes Nothing) (map Just . MEM.toBytes) s
     in assert (nbytes == length sbytes) $
          zipWith Concolic cbytes sbytes

  fromBytes ty bytes =
    do
      let conBytes = map concrete bytes
      con <- MEM.fromBytes ty conBytes

      let mkConcolic = Concolic con
      if any hasSymbolic bytes
        then do
          let symBVs = map (getSymbolicDef SE.fromByte) bytes
          MEM.fromBytes ty symBVs <&> mkConcolic . Just
        else Just $ mkConcolic Nothing

------------------------------------------------------------------------

unaryOp ::
  (D.RegVal -> Maybe D.RegVal) ->
  (SE.BitVector -> Maybe SE.BitVector) ->
  Concolic D.RegVal ->
  Maybe (Concolic D.RegVal)
unaryOp fnCon fnSym Concolic {concrete = c, symbolic = s} = do
  c' <- fnCon c
  let con = Concolic c'
  case s of
    Just s' -> fnSym s' <&> con . Just
    Nothing -> pure $ con Nothing

binaryOp ::
  (D.RegVal -> D.RegVal -> Maybe D.RegVal) ->
  (SE.BitVector -> SE.BitVector -> Maybe SE.BitVector) ->
  Concolic D.RegVal ->
  Concolic D.RegVal ->
  Maybe (Concolic D.RegVal)
binaryOp fnCon fnSym lhs rhs =
  do
    c <- concrete lhs `fnCon` concrete rhs
    if hasSymbolic lhs || hasSymbolic rhs
      then
        let lhsS = getSymbolicDef SE.fromReg lhs
            rhsS = getSymbolicDef SE.fromReg rhs
         in (lhsS `fnSym` rhsS) <&> Concolic c . Just
      else pure $ Concolic c Nothing

instance E.ValueRepr (Concolic D.RegVal) where
  fromLit ty v = Concolic (E.fromLit ty v) Nothing
  fromFloat fl = Concolic (E.fromFloat fl) Nothing
  fromDouble d = Concolic (E.fromDouble d) Nothing

  fromAddress addr = Concolic (E.fromAddress addr) Nothing
  toAddress Concolic {concrete = c} = E.toAddress c
  isZero Concolic {concrete = c} = E.isZero c

  add = binaryOp E.add E.add
  sub = binaryOp E.sub E.sub
  mul = binaryOp E.mul E.mul
  urem = binaryOp E.urem E.urem
  srem = binaryOp E.srem E.srem
  udiv = binaryOp E.udiv E.udiv

  eq = binaryOp E.eq E.eq
  ne = binaryOp E.ne E.ne
  sle = binaryOp E.sle E.sle
  slt = binaryOp E.slt E.slt
  sge = binaryOp E.sge E.sge
  sgt = binaryOp E.sgt E.sgt
  ule = binaryOp E.ule E.ule
  ult = binaryOp E.ult E.ult
  uge = binaryOp E.uge E.uge
  ugt = binaryOp E.ugt E.ugt

  wordToLong ty = unaryOp (E.wordToLong ty) (E.wordToLong ty)
  subType ty = unaryOp (E.subType ty) (E.subType ty)
