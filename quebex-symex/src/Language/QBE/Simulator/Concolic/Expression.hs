module Language.QBE.Simulator.Concolic.Expression
  ( Concolic (..),
    unconstrained,
    hasSymbolic,
  )
where

import Control.Exception (assert)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Symbolic (bitSize)
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT

data Concolic
  = Concolic
  { concrete :: D.RegVal,
    symbolic :: Maybe SE.BitVector
  }
  deriving (Show)

unconstrained :: SMT.Solver -> String -> QBE.BaseType -> IO Concolic
unconstrained solver name ty = do
  s <- SMT.declare solver name (SMT.tBits numBits)
  -- TODO: Random value for concrete part
  return $ Concolic (E.fromLit ty 0) (Just $ SE.fromSExpr ty s)
  where
    numBits :: Integer
    numBits = fromIntegral $ bitSize (QBE.Base ty)

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

  extend ty = unaryOp (E.extend ty) (E.extend ty)
  subType ty = unaryOp (E.subType ty) (E.subType ty)
