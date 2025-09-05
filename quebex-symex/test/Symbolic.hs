-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# OPTIONS_GHC -Wno-x-partial #-}

module Symbolic (exprTests) where

import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Explorer (defSolver)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Types qualified as QBE
import SimpleSMT qualified as SMT
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

------------------------------------------------------------------------

data ShiftInput = ShiftInput QBE.BaseType Word64 Word32
  deriving (Show)

instance Arbitrary ShiftInput where
  arbitrary = do
    t <- elements [QBE.Word, QBE.Long]
    v <- arbitrary
    s <- arbitrary
    pure $ ShiftInput t v s

eqConcrete :: SE.BitVector -> DE.RegVal -> IO Bool
eqConcrete sym con = do
  s <- getSolver
  symVal <- SMT.getExpr s (SE.toSExpr sym)
  case (symVal, con) of
    (SMT.Bits 32 sv, DE.VWord cv) -> pure $ sv == (fromIntegral cv)
    (SMT.Bits 64 sv, DE.VLong cv) -> pure $ sv == (fromIntegral cv)
    _ -> pure False

shiftProp ::
  (SE.BitVector -> SE.BitVector -> Maybe SE.BitVector) ->
  (DE.RegVal -> DE.RegVal -> Maybe DE.RegVal) ->
  ShiftInput ->
  Property
shiftProp opSym opCon (ShiftInput ty val amount) = ioProperty $ do
  let symValue = E.fromLit ty val :: SE.BitVector
  let conValue = E.fromLit ty val :: DE.RegVal

  let symResult = symValue `opSym` (E.fromLit QBE.Word $ fromIntegral amount)
  let conResult = conValue `opCon` (E.fromLit QBE.Word $ fromIntegral amount)

  case (symResult, conResult) of
    (Just s, Just c) -> eqConcrete s c
    (Nothing, Nothing) -> pure True
    _ -> pure False

shiftEquiv :: TestTree
shiftEquiv =
  testGroup
    "Concrete and symbolic shifts are equivalent"
    [ testProperty "sar" (shiftProp E.sar E.sar),
      testProperty "shr" (shiftProp E.shr E.shr),
      testProperty "shl" (shiftProp E.shl E.shl)
    ]

------------------------------------------------------------------------

storeTests :: TestTree
storeTests =
  testGroup
    "Storage Instance Tests"
    [ testCase "Create bitvector and convert it to bytes" $
        do
          s <- getSolver
          let bytes = (MEM.toBytes (E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector) :: [SE.BitVector])
          values <- mapM (SMT.getExpr s . SE.toSExpr) bytes
          values @?= [SMT.Bits 8 0xef, SMT.Bits 8 0xbe, SMT.Bits 8 0xad, SMT.Bits 8 0xde],
      testCase "Convert bitvector to bytes and back" $
        do
          s <- getSolver

          let bytes = (MEM.toBytes (E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector) :: [SE.BitVector])
          length bytes @?= 4

          value <- case MEM.fromBytes (QBE.LBase QBE.Word) bytes of
            Just x -> SMT.getExpr s (SE.toSExpr x) <&> Just
            Nothing -> pure Nothing
          value @?= Just (SMT.Bits 32 0xdeadbeef)
    ]

valueReprTests :: TestTree
valueReprTests =
  testGroup
    "Symbolic ValueRepr Tests"
    [ testCase "Create from literal and add" $
        do
          s <- getSolver

          let v1 = E.fromLit QBE.Word 127
          let v2 = E.fromLit QBE.Word 128

          expr <- SMT.getExpr s (SE.toSExpr $ fromJust $ v1 `E.add` v2)
          expr @?= SMT.Bits 32 0xff,
      testCase "Add incompatible values" $
        do
          let v1 = E.fromLit QBE.Word 0xffffffff :: SE.BitVector
          let v2 = E.fromLit QBE.Long 0xff :: SE.BitVector

          -- Note: E.add doesn't do subtyping if invoked directly
          (v1 `E.add` v2) @?= Nothing,
      testCase "Subtyping" $
        do
          s <- getSolver

          let v1 = E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector
          let v2 = E.fromLit QBE.Long 0xff :: SE.BitVector

          let v1sub = fromJust $ E.subType QBE.Word v1
          v1sub' <- SMT.getExpr s (SE.toSExpr v1sub)
          v1sub' @?= SMT.Bits 32 0xdeadbeef

          let v2sub = fromJust $ E.subType QBE.Word v2
          v2sub' <- SMT.getExpr s (SE.toSExpr v2sub)
          v2sub' @?= SMT.Bits 32 0xff

          let subtypedAddExpr = v1sub `E.add` v2sub
          expr <- SMT.getExpr s (SE.toSExpr (fromJust subtypedAddExpr))

          expr @?= SMT.Bits 32 0xdeadbfee,
      testCase "Extend subwords" $
        do
          s <- getSolver

          let value = E.fromLit QBE.Word 0xdeadbeef :: SE.BitVector

          -- let sext = fromJust $ E.wordToLong (QBE.SLSubWord QBE.SignedByte) value
          -- sextVal <- SMT.getExpr s (SE.toSExpr sext)
          -- sextVal @?= SMT.Bits 64 0xffffffffffffffef

          let zext = fromJust $ E.wordToLong (QBE.SLSubWord QBE.UnsignedByte) value
          zextVal <- SMT.getExpr s (SE.toSExpr zext)
          zextVal @?= SMT.Bits 64 0xef
    ]

exprTests :: TestTree
exprTests = testGroup "Expression tests" [storeTests, valueReprTests, shiftEquiv]
