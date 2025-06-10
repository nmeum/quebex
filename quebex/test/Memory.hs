module Memory (memTests) where

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
        let bs = [0x0, 0x0, 0x0, 0x0]
        loadBytes m 0x0 4 >>= assertEqual "" bs,
      testCase "Read entire memory" $ do
        m <- mkMemory 0 128
        let bs = replicate 128 0
        loadBytes m 0x0 128 >>= assertEqual "" bs,
      testCase "Store and read byte" $ do
        m <- mkMemory 0 64
        storeBytes m 0x0 [0xff]
        loadBytes m 0x0 1 >>= assertEqual "" [0xff],
      testCase "Store and read bytes" $ do
        m <- mkMemory 0 32
        storeBytes m 0x0 [0xde, 0xad, 0xbe, 0xef]
        loadBytes m 0x0 4 >>= assertEqual "" [0xde, 0xad, 0xbe, 0xef],
      testCase "Store and read multiple bytes" $ do
        m <- mkMemory 0 4
        storeBytes m 0x0 [0xde, 0xad]
        storeBytes m 0x2 [0xbe, 0xef]
        loadBytes m 0x0 2 >>= assertEqual "" [0xde, 0xad]
        loadBytes m 0x2 2 >>= assertEqual "" [0xbe, 0xef]
        loadBytes m 0x0 4 >>= assertEqual "" [0xde, 0xad, 0xbe, 0xef]
    ]
