-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Memory (memTests) where

import Data.Array.IO (IOUArray)
import Data.Word (Word8)
import Language.QBE.Simulator.Memory
import Test.Tasty
import Test.Tasty.HUnit

memTests :: TestTree
memTests =
  testGroup
    "Memory tests"
    [ testCase "Create memory and extract its size" $ do
        mem <- mkMemory 0x0 512 :: IO (Memory IOUArray Word8)
        memSize mem >>= assertEqual "" 512,
      testCase "Store and read byte" $ do
        m <- mkMemory 0 64 :: IO (Memory IOUArray Word8)
        storeBytes m 0x0 [0xff]
        loadBytes m 0x0 1 >>= assertEqual "" [0xff],
      testCase "Store and read bytes" $ do
        m <- mkMemory 0 32 :: IO (Memory IOUArray Word8)
        storeBytes m 0x0 [0xde, 0xad, 0xbe, 0xef]
        loadBytes m 0x0 4 >>= assertEqual "" [0xde, 0xad, 0xbe, 0xef],
      testCase "Store and read multiple bytes" $ do
        m <- mkMemory 0 4 :: IO (Memory IOUArray Word8)
        storeBytes m 0x0 [0xde, 0xad]
        storeBytes m 0x2 [0xbe, 0xef]
        loadBytes m 0x0 2 >>= assertEqual "" [0xde, 0xad]
        loadBytes m 0x2 2 >>= assertEqual "" [0xbe, 0xef]
        loadBytes m 0x0 4 >>= assertEqual "" [0xde, 0xad, 0xbe, 0xef]
    ]
