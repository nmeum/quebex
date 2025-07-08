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
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Simulator.Symbolic.Tracer (ExecTrace, appendBranch, newBranch)
import Language.QBE.Simulator.Tracer (Tracer (..))

data Concolic
  = Concolic
  { concrete :: D.RegVal,
    symbolic :: Maybe SE.BitVector
  }
  deriving (Show)

hasSymbolic :: Concolic -> Bool
hasSymbolic Concolic {symbolic = Just _} = True
hasSymbolic _ = False

getSymbolicDef :: Concolic -> SE.BitVector
getSymbolicDef Concolic {concrete = c, symbolic = s} =
  fromMaybe (SE.fromReg c) s

------------------------------------------------------------------------

instance E.Storable Concolic where
  toBytes Concolic {concrete = c, symbolic = s} =
    let cbytes = E.toBytes c
        nbytes = length cbytes
        sbytes = maybe (replicate nbytes Nothing) (map Just . E.toBytes) s
     in assert (nbytes == length sbytes) $
          zipWith Concolic cbytes sbytes

  fromBytes ty bytes =
    do
      let conBytes = map concrete bytes
      con <- E.fromBytes ty conBytes

      let mkConcolic = Concolic con
      if any hasSymbolic bytes
        then do
          let symBVs = map getSymbolicDef bytes
          E.fromBytes ty symBVs <&> mkConcolic . Just
        else Just $ mkConcolic Nothing

instance Tracer ExecTrace Concolic where
  branch t Concolic {symbolic = Just s} condResult =
    appendBranch t condResult (newBranch s)
  branch t _ _ = t

------------------------------------------------------------------------

unaryOp ::
  (D.RegVal -> Maybe D.RegVal) ->
  (SE.BitVector -> Maybe SE.BitVector) ->
  Concolic ->
  Maybe Concolic
unaryOp fnCon fnSym Concolic {concrete = c, symbolic = s} = do
  c' <- fnCon c
  let con = Concolic c'
  case s of
    Just s' -> fnSym s' <&> con . Just
    Nothing -> pure $ con Nothing

binaryOp ::
  (D.RegVal -> D.RegVal -> Maybe D.RegVal) ->
  (SE.BitVector -> SE.BitVector -> Maybe SE.BitVector) ->
  Concolic ->
  Concolic ->
  Maybe Concolic
binaryOp fnCon fnSym lhs rhs =
  do
    c <- concrete lhs `fnCon` concrete rhs
    if hasSymbolic lhs || hasSymbolic rhs
      then
        let lhsS = getSymbolicDef lhs
            rhsS = getSymbolicDef rhs
         in (lhsS `fnSym` rhsS) <&> Concolic c . Just
      else pure $ Concolic c Nothing

instance E.ValueRepr Concolic where
  fromLit ty v = Concolic (E.fromLit ty v) Nothing
  fromFloat fl = Concolic (E.fromFloat fl) Nothing
  fromDouble d = Concolic (E.fromDouble d) Nothing

  fromAddress addr = Concolic (E.fromAddress addr) Nothing
  toAddress Concolic {concrete = c} = E.toAddress c
  isZero Concolic {concrete = c} = E.isZero c

  add = binaryOp E.add E.add
  sub = binaryOp E.sub E.sub
  mul = binaryOp E.mul E.mul

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

  swToLong ty = unaryOp (E.swToLong ty) (E.swToLong ty)
  wordToLong ty = unaryOp (E.wordToLong ty) (E.wordToLong ty)
  subType ty = unaryOp (E.subType ty) (E.subType ty)
