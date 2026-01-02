-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Symbolic (exprTests) where

import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Explorer (defSolver)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleBV qualified as SMT
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
  ( Arbitrary,
    Property,
    arbitrary,
    elements,
    ioProperty,
    testProperty,
  )

getSolver :: IO SMT.Solver
getSolver = do
  s <- defSolver
  SMT.check s >> pure s

eqConcrete :: Maybe SE.BitVector -> Maybe DE.RegVal -> IO Bool
eqConcrete (Just sym) (Just con) = do
  s <- getSolver
  symVal <- SMT.getValue s (SE.toSExpr sym)
  case (symVal, con) of
    (SMT.Bits 32 sv, DE.VWord cv) -> pure $ sv == fromIntegral cv
    (SMT.Bits 64 sv, DE.VLong cv) -> pure $ sv == fromIntegral cv
    _ -> pure False
eqConcrete Nothing Nothing = pure True
eqConcrete _ _ = pure False

------------------------------------------------------------------------

data UnaryInput = UnaryInput QBE.BaseType Word64
  deriving (Show)

instance Arbitrary UnaryInput where
  arbitrary = do
    t <- elements [QBE.Word, QBE.Long]
    UnaryInput t <$> arbitrary

unaryProp ::
  (SE.BitVector -> Maybe SE.BitVector) ->
  (DE.RegVal -> Maybe DE.RegVal) ->
  UnaryInput ->
  Property
unaryProp opSym opCon (UnaryInput ty val) = ioProperty $ do
  eqConcrete (opSym $ E.fromLit (QBE.Base ty) val) (opCon $ E.fromLit (QBE.Base ty) val)

negEquiv :: TestTree
negEquiv = testProperty "neg" (unaryProp E.neg E.neg)

------------------------------------------------------------------------

data BinaryInput = BinaryInput QBE.BaseType Word64 Word64
  deriving (Show)

instance Arbitrary BinaryInput where
  arbitrary = do
    ty <- elements [QBE.Word, QBE.Long]
    lhs <- arbitrary
    BinaryInput ty lhs <$> arbitrary

binaryEq ::
  (SE.BitVector -> SE.BitVector -> Maybe SE.BitVector) ->
  (DE.RegVal -> DE.RegVal -> Maybe DE.RegVal) ->
  BinaryInput ->
  IO Bool
binaryEq opSym opCon (BinaryInput ty lhs rhs) =
  eqConcrete (opSym (mkS lhs) (mkS rhs)) (opCon (mkC lhs) (mkC rhs))
  where
    mkS :: Word64 -> SE.BitVector
    mkS = E.fromLit (QBE.Base ty)

    mkC :: Word64 -> DE.RegVal
    mkC = E.fromLit (QBE.Base ty)

binaryProp ::
  (SE.BitVector -> SE.BitVector -> Maybe SE.BitVector) ->
  (DE.RegVal -> DE.RegVal -> Maybe DE.RegVal) ->
  BinaryInput ->
  Property
binaryProp opSym opCon input = ioProperty $ binaryEq opSym opCon input

opEquiv :: TestTree
opEquiv =
  testGroup
    "Operation equivalence"
    [ testProperty "add" (binaryProp E.add E.add),
      testProperty "sub" (binaryProp E.sub E.sub),
      testProperty "mul" (binaryProp E.mul E.mul),
      testProperty "div" (binaryProp E.div E.div),
      testProperty "or" (binaryProp E.or E.or),
      testProperty "xor" (binaryProp E.xor E.xor),
      testProperty "and" (binaryProp E.and E.and),
      testProperty "urem" (binaryProp E.urem E.urem),
      testProperty "srem" (binaryProp E.srem E.srem),
      testProperty "udiv" (binaryProp E.udiv E.udiv),
      testProperty "eq" (binaryProp E.eq E.eq),
      testProperty "ne" (binaryProp E.ne E.ne),
      testProperty "sle" (binaryProp E.sle E.sle),
      testProperty "slt" (binaryProp E.slt E.slt),
      testProperty "sge" (binaryProp E.sge E.sge),
      testProperty "sgt" (binaryProp E.sgt E.sgt),
      testProperty "ule" (binaryProp E.ule E.ule),
      testProperty "ult" (binaryProp E.ult E.ult),
      testProperty "uge" (binaryProp E.uge E.uge),
      testProperty "ugt" (binaryProp E.ugt E.ugt)
    ]

------------------------------------------------------------------------

data ShiftInput = ShiftInput QBE.BaseType Word64 Word32
  deriving (Show)

instance Arbitrary ShiftInput where
  arbitrary = do
    t <- elements [QBE.Word, QBE.Long]
    v <- arbitrary
    ShiftInput t v <$> arbitrary

shiftProp ::
  (SE.BitVector -> SE.BitVector -> Maybe SE.BitVector) ->
  (DE.RegVal -> DE.RegVal -> Maybe DE.RegVal) ->
  ShiftInput ->
  Property
shiftProp opSym opCon (ShiftInput ty val amount) = ioProperty $ do
  let symValue = E.fromLit (QBE.Base ty) val :: SE.BitVector
  let conValue = E.fromLit (QBE.Base ty) val :: DE.RegVal

  let symResult = symValue `opSym` E.fromLit (QBE.Base QBE.Word) (fromIntegral amount)
  let conResult = conValue `opCon` E.fromLit (QBE.Base QBE.Word) (fromIntegral amount)

  eqConcrete symResult conResult

shiftEquiv :: TestTree
shiftEquiv =
  testGroup
    "Concrete and symbolic shifts are equivalent"
    [ testProperty "sar" (shiftProp E.sar E.sar),
      testProperty "shr" (shiftProp E.shr E.shr),
      testProperty "shl" (shiftProp E.shl E.shl)
    ]

------------------------------------------------------------------------

equivTests :: TestTree
equivTests =
  testGroup
    "Equivalence tests for symbolic and default expression interpreter"
    [ shiftEquiv,
      negEquiv,
      opEquiv,
      -- Occurs when the most-negative integer is divided by -1.
      testCase "Signed division overflow on div" $
        do
          let input =
                BinaryInput QBE.Long 0x8000000000000000 $
                  fromIntegral (-1 :: Int64)
          binaryEq E.div E.div input >>= assertBool "signed-div-overflow",
      testCase "Signed division overflow on srem" $
        do
          let input =
                BinaryInput QBE.Long 0x8000000000000000 $
                  fromIntegral (-1 :: Int64)
          binaryEq E.srem E.srem input >>= assertBool "signed-srem-overflow"
    ]

storeTests :: TestTree
storeTests =
  testGroup
    "Storage Instance Tests"
    [ testCase "Create bitvector and convert it to bytes" $
        do
          s <- getSolver
          let bytes = (MEM.toBytes (E.fromLit (QBE.Base QBE.Word) 0xdeadbeef :: SE.BitVector) :: [SE.BitVector])
          values <- mapM (SMT.getValue s . SE.toSExpr) bytes
          values @?= [SMT.Bits 8 0xef, SMT.Bits 8 0xbe, SMT.Bits 8 0xad, SMT.Bits 8 0xde],
      testCase "Convert bitvector to bytes and back" $
        do
          s <- getSolver

          let bytes = (MEM.toBytes (E.fromLit (QBE.Base QBE.Word) 0xdeadbeef :: SE.BitVector) :: [SE.BitVector])
          length bytes @?= 4

          value <- case MEM.fromBytes (QBE.LBase QBE.Word) bytes of
            Just x -> SMT.getValue s (SE.toSExpr x) <&> Just
            Nothing -> pure Nothing
          value @?= Just (SMT.Bits 32 0xdeadbeef)
    ]

valueReprTests :: TestTree
valueReprTests =
  testGroup
    "Symbolic ValueRepr Tests"
    [ testCase "create from literal and add" $
        do
          s <- getSolver

          let v1 = E.fromLit (QBE.Base QBE.Word) 127
          let v2 = E.fromLit (QBE.Base QBE.Word) 128

          expr <- SMT.getValue s (SE.toSExpr $ fromJust $ v1 `E.add` v2)
          expr @?= SMT.Bits 32 0xff,
      testCase "add incompatible values" $
        do
          let v1 = E.fromLit (QBE.Base QBE.Word) 0xffffffff :: SE.BitVector
          let v2 = E.fromLit (QBE.Base QBE.Long) 0xff :: SE.BitVector

          -- Note: E.add doesn't do subtyping if invoked directly
          (v1 `E.add` v2) @?= Nothing,
      testCase "extend" $
        do
          s <- getSolver

          let v1 = E.fromLit QBE.Byte 0xff :: SE.BitVector
              ext1 = fromJust $ E.extend QBE.HalfWord False v1
          ext1Val <- SMT.getValue s (SE.toSExpr ext1)
          ext1Val @?= SMT.Bits 16 0x00ff

          let v2 = E.fromLit QBE.Byte 0xab :: SE.BitVector
              ext2 = fromJust $ E.extend (QBE.Base QBE.Word) True v2
          ext2Val <- SMT.getValue s (SE.toSExpr ext2)
          ext2Val @?= SMT.Bits 32 0xffffffab

          let v3 = E.fromLit (QBE.Base QBE.Word) 0xdeadbeef :: SE.BitVector
          E.extend (QBE.Base QBE.Word) True v3 @?= Nothing
          E.extend QBE.Byte True v3 @?= Nothing,
      testCase "extract" $
        do
          s <- getSolver

          let value = E.fromLit (QBE.Base QBE.Word) 0xdeadbeef :: SE.BitVector

          let ex1 = fromJust $ E.extract QBE.Byte value
          ex1Val <- SMT.getValue s (SE.toSExpr ex1)
          ex1Val @?= SMT.Bits 8 0xef

          let ex2 = fromJust $ E.extract QBE.HalfWord value
          ex2Val <- SMT.getValue s (SE.toSExpr ex2)
          ex2Val @?= SMT.Bits 16 0xbeef

          E.extract (QBE.Base QBE.Word) value @?= Just value
          E.extract (QBE.Base QBE.Long) value @?= Nothing
    ]

exprTests :: TestTree
exprTests = testGroup "Expression tests" [storeTests, valueReprTests, equivTests]
