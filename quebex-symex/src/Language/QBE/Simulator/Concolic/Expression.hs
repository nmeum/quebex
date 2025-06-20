module Language.QBE.Simulator.Concolic.Expression
  ( Concolic (concrete, symbolic),
    unconstrained,
    hasSymbolic,
  )
where

import Control.Exception (assert)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
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

data ConcolicByte
  = ConcolicByte
  { concreteByte :: Word8,
    symbolicByte :: Maybe SE.BitVector
  }
  deriving (Show)

hasSymbolicByte :: ConcolicByte -> Bool
hasSymbolicByte ConcolicByte {symbolicByte = Just _} = True
hasSymbolicByte _ = False

getSymbolicDefByte :: ConcolicByte -> SE.BitVector
getSymbolicDefByte ConcolicByte {concreteByte = cb, symbolicByte = sb} =
  fromMaybe (SE.fromByte cb) sb

instance E.Storable Concolic ConcolicByte where
  toBytes Concolic {concrete = c, symbolic = s} =
    let cbytes = E.toBytes c
        nbytes = length cbytes
        sbytes = maybe (replicate nbytes Nothing) (map Just . E.toBytes) s
     in assert (nbytes == length sbytes) $
          zipWith ConcolicByte cbytes sbytes

  fromBytes ty bytes =
    do
      let conBytes = map concreteByte bytes
      con <- E.fromBytes ty conBytes

      let mkConcolic = Concolic con
      if any hasSymbolicByte bytes
        then do
          let symBVs = map getSymbolicDefByte bytes
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
