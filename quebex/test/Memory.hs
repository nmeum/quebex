module Memory (memTests) where

import Data.ByteString.Lazy qualified as BSL
import Language.QBE.Simulator.Memory
import Test.Tasty
import Test.Tasty.HUnit

memTests :: TestTree
memTests =
  testGroup
    "Memory tests"
    [ testCase "Create memory and extract its size" $ do
        mem <- mkMemory 0x0 512
        memSize mem >>= assertEqual "" 512,
      testCase "Read uninitialized memory" $ do
        m <- mkMemory 0x0 256
        let bs = BSL.pack [0x0, 0x0, 0x0, 0x0]
        loadByteString m 0x0 4 >>= assertEqual "" bs,
      testCase "Read entire memory" $ do
        m <- mkMemory 0 128
        let bs = BSL.pack $ replicate 128 0
        loadByteString m 0x0 128 >>= assertEqual "" bs,
      testCase "Store and read byte" $ do
        m <- mkMemory 0 64
        storeByteString m 0x0 $ BSL.pack [0xff]
        loadByteString m 0x0 1 >>= assertEqual "" (BSL.pack [0xff])
    ]
