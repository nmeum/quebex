module Language.QBE.Simulator.Concolic.Expression
  ( Concolic (concrete, symbolic),
    hasSymbolic,
  )
where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic.Expression qualified as SE

data Concolic
  = Concolic
  { concrete :: D.RegVal,
    symbolic :: Maybe SE.BitVector
  }

hasSymbolic :: Concolic -> Bool
hasSymbolic Concolic {symbolic = Just _} = True
hasSymbolic _ = False

getSymbolicDef :: Concolic -> SE.BitVector
getSymbolicDef Concolic {concrete = c, symbolic = s} =
  fromMaybe (SE.fromReg c) s

-- TODO: Storable instance

------------------------------------------------------------------------

unaryOp ::
  (D.RegVal -> Maybe D.RegVal) ->
  (SE.BitVector -> Maybe SE.BitVector) ->
  Concolic ->
  Maybe Concolic
unaryOp fnCon fnSym Concolic {concrete = c, symbolic = s} = do
  c' <- fnCon c
  s' <- s <&> fnSym
  return Concolic {concrete = c', symbolic = s'}

binaryOp ::
  (D.RegVal -> D.RegVal -> Maybe D.RegVal) ->
  (SE.BitVector -> SE.BitVector -> Maybe SE.BitVector) ->
  Concolic ->
  Concolic ->
  Maybe Concolic
binaryOp fnCon fnSym lhs rhs =
  do
    c <- (concrete lhs) `fnCon` (concrete rhs)
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

  extend ty v = unaryOp (E.extend ty) (E.extend ty) v
  subType ty v = unaryOp (E.subType ty) (E.subType ty) v
  isZero Concolic {concrete = c} = E.isZero c

  add = binaryOp E.add E.add
  sub = binaryOp E.sub E.sub
