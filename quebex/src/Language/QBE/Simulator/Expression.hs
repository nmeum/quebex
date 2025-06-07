module Language.QBE.Simulator.Expression where

import Data.ByteString.Builder qualified as B
import Data.Word (Word16, Word32, Word64, Word8)
import Language.QBE.Simulator.Error
import Language.QBE.Types qualified as QBE

data RegVal
  = EByte Word8
  | EHalf Word16
  | EWord Word32
  | ELong Word64
  | ESingle Float
  | EDouble Double
  deriving (Show, Eq)

toBuilder :: RegVal -> B.Builder
toBuilder (EByte w) = B.word8 w
toBuilder (EHalf w) = B.word16LE w
toBuilder (EWord w) = B.word32LE w
toBuilder (ELong w) = B.word64LE w
toBuilder (ESingle w) = B.floatLE w
toBuilder (EDouble w) = B.doubleLE w

------------------------------------------------------------------------

addVals :: RegVal -> RegVal -> Either EvalError RegVal
addVals (EByte lhs) (EByte rhs) = Right (EByte $ lhs + rhs)
addVals (EHalf lhs) (EHalf rhs) = Right (EHalf $ lhs + rhs)
addVals (EWord lhs) (EWord rhs) = Right (EWord $ lhs + rhs)
addVals (ELong lhs) (ELong rhs) = Right (ELong $ lhs + rhs)
addVals (ESingle lhs) (ESingle rhs) = Right (ESingle $ lhs + rhs)
addVals (EDouble lhs) (EDouble rhs) = Right (EDouble $ lhs + rhs)
addVals _ _ = Left TypingError

subVals :: RegVal -> RegVal -> Either EvalError RegVal
subVals (EByte lhs) (EByte rhs) = Right (EByte $ lhs - rhs)
subVals (EHalf lhs) (EHalf rhs) = Right (EHalf $ lhs - rhs)
subVals (EWord lhs) (EWord rhs) = Right (EWord $ lhs - rhs)
subVals (ELong lhs) (ELong rhs) = Right (ELong $ lhs - rhs)
subVals (ESingle lhs) (ESingle rhs) = Right (ESingle $ lhs - rhs)
subVals (EDouble lhs) (EDouble rhs) = Right (EDouble $ lhs - rhs)
subVals _ _ = Left TypingError

------------------------------------------------------------------------

assertType :: QBE.BaseType -> RegVal -> Either EvalError RegVal
assertType QBE.Word v@(EWord _) = Right v
assertType QBE.Long v@(ELong _) = Right v
assertType QBE.Single v@(ESingle _) = Right v
assertType QBE.Double v@(EDouble _) = Right v
assertType _ _ = Left TypingError
