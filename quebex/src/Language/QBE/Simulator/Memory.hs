-- SPDX-FileCopyrightText: 2023-2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

-- | This module provides an implementation of a simple byte-addressable memory
-- based on "Data.Array".
module Language.QBE.Simulator.Memory
  ( -- * Type Aliases
    Address,
    Size,
    showAddr,

    -- * Value Representation
    Storable (toBytes, fromBytes),

    -- * Memory Representation
    Memory,
    mkMemory,
    memSize,
    loadBytes,
    storeBytes,

    -- * Memory Address
    toMemAddr,
    addrOverlap,
    alignAddr,
  )
where

import Data.Array.IO
  ( MArray,
    getBounds,
    newArray_,
    readArray,
    writeArray,
  )
import Data.Bits (complement, (.&.))
import Data.Word (Word64)
import Language.QBE.Types qualified as QBE
import Numeric (showHex)

-- | Type used to represent an address in memory.
type Address = Word64

-- | Type used to represent the memory's size.
type Size = Word64

-- | Represent an address as a hexadecimal string.
showAddr :: Address -> String
showAddr addr = "0x" ++ showHex addr ""

------------------------------------------------------------------------

-- | Type class for types that can be stored in memory. That is, types whose
-- values can be converted to the given byte representation and vice versa.
class Storable valTy byteTy where
  -- | Convert a value type to a list of byte types.
  toBytes :: valTy -> [byteTy]

  -- | Convert a list of bytes to a value type of 'QBE.LoadType'. Returns
  -- 'Nothing' if the length of the list is incompatible with the given
  -- 'QBE.LoadType'.
  fromBytes :: QBE.LoadType -> [byteTy] -> Maybe valTy

-- | Memory parameterized over the Array type (e.g. 'Data.Array.IO.IOUArray')
-- and a byte polymorphic representation (e.g. 'Word8').
data Memory a v = Memory
  { memStart :: Address,
    memBytes :: a Address v
  }

-- | Create a new 'Memory' which starts at the given base address and
-- has a maximum capacity (i.e., can store up to the given amount of bytes).
-- The memory is not initialized, reading an uninitialized values results
-- in an error.
mkMemory :: (MArray t a IO) => Address -> Size -> IO (Memory t a)
mkMemory startAddr size = do
  ary <- newArray_ (0, size - 1)
  return $ Memory startAddr ary

-- | Translate global address to a memory-local address. That is, performs
-- address translation relative to the base address of the 'Memory'.
toMemAddr :: Memory t a -> Address -> Address
toMemAddr mem addr = addr - memStart mem

-- | Returns true if the given addresses, passed in the first and second
-- argument, overlap in the given range (i.e., the given amount of bytes).
addrOverlap :: Address -> Address -> Size -> Bool
addrOverlap addr1 addr2 range =
  addr1 < endAddr addr2 && endAddr addr1 > addr2
  where
    endAddr :: Address -> Address
    endAddr a = a + range

-- | Align an address upwards for the given alignment.
alignAddr :: Address -> Size -> Address
alignAddr addr align = (addr + (align - 1)) .&. complement (align - 1)

-- | Returns the size of the memory in bytes.
memSize :: (MArray t a IO) => Memory t a -> IO Size
memSize = fmap ((+ 1) . snd) . getBounds . memBytes

-- | Write the list of bytes to memory at the given address.
storeBytes :: (MArray t a IO) => Memory t a -> Address -> [a] -> IO ()
storeBytes mem addr bytes =
  mapM_ (\(off, val) -> storeByte mem (addr + off) val) $
    zip [0 ..] bytes
  where
    storeByte :: (MArray t a IO) => Memory t a -> Address -> a -> IO ()
    storeByte m a = writeArray (memBytes m) $ toMemAddr mem a
{-# INLINEABLE storeBytes #-}

-- | Load the given amount of bytes at the given address.
loadBytes :: (MArray t a IO) => Memory t a -> Address -> Size -> IO [a]
loadBytes mem addr byteSize =
  mapM (\off -> loadByte mem (addr + off)) [0 .. byteSize - 1]
  where
    loadByte :: (MArray t a IO) => Memory t a -> Address -> IO a
    loadByte m = readArray (memBytes m) . toMemAddr m
{-# INLINEABLE loadBytes #-}
