module Concolic (exprTests) where

import Data.Maybe (fromJust)
import Language.QBE.Simulator.Concolic.Expression qualified as C
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

-- TODO: QuickCheck tests against the default interpreter's implementation.
storeTests :: TestTree
storeTests =
  testGroup
    "Storage Instance Tests"
    -- TODO: Test case for partial concoilc bytes
    [ testCase "Convert concrete concolic value to bytes and back" $
        do
          let value = E.fromLit QBE.Word 0xdeadbeef :: C.Concolic

          let bytes = E.toBytes value
          length bytes @?= 4

          let valueFromBytes = fromJust $ E.fromBytes (QBE.Base QBE.Word) bytes
          C.concrete value @?= C.concrete valueFromBytes
    ]

exprTests :: TestTree
exprTests = testGroup "Expression tests" [storeTests]
