-- SPDX-FileCopyrightText: 2023-2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.Simulator.Memory
  ( Address,
    Size,
    Memory (..),
    mkMemory,
    toMemAddr,
    addrOverlap,
    memSize,
    loadBytes,
    storeBytes,
    Storable (toBytes, fromBytes),
  )
where

import Data.Array.IO
  ( MArray,
    getBounds,
    newArray_,
    readArray,
    writeArray,
  )
import Data.Word (Word64)
import Language.QBE.Simulator.Expression (Address)
import Language.QBE.Types qualified as QBE

class Storable valTy byteTy where
  toBytes :: valTy -> [byteTy]
  fromBytes :: QBE.LoadType -> [byteTy] -> Maybe valTy

-- | Type used to represent the memory's size.
type Size = Word64

-- | Memory parameterized over the Array type (e.g. 'IOUArray') and a byte
-- polymorphic representation (e.g. 'Word8').
data Memory a v = Memory
  { memStart :: Address,
    memBytes :: a Address v
  }

------------------------------------------------------------------------

mkMemory :: (MArray t a IO) => Address -> Size -> IO (Memory t a)
mkMemory startAddr size = do
  ary <- newArray_ (0, size - 1)
  return $ Memory startAddr ary

-- Translate global address to a memory-local address.
toMemAddr :: Memory t a -> Address -> Address
toMemAddr mem addr = addr - memStart mem

-- Returns true of the given addresses overlap in the given range.
addrOverlap :: Address -> Address -> Address -> Bool
addrOverlap addr1 addr2 range =
  addr1 < endAddr addr2 && endAddr addr1 > addr2
  where
    endAddr :: Address -> Address
    endAddr a = a + range

-- | Returns the size of the memory in bytes.
memSize :: (MArray t a IO) => Memory t a -> IO Size
memSize = fmap ((+ 1) . snd) . getBounds . memBytes

storeBytes :: (MArray t a IO) => Memory t a -> Address -> [a] -> IO ()
storeBytes mem addr bytes =
  mapM_ (\(off, val) -> storeByte mem (addr + off) val) $
    zip [0 ..] bytes
  where
    storeByte :: (MArray t a IO) => Memory t a -> Address -> a -> IO ()
    storeByte m a = writeArray (memBytes m) $ toMemAddr mem a
{-# INLINEABLE storeBytes #-}

loadBytes :: (MArray t a IO) => Memory t a -> Address -> Size -> IO [a]
loadBytes mem addr byteSize =
  mapM (\off -> loadByte mem (addr + off)) [0 .. byteSize - 1]
  where
    loadByte :: (MArray t a IO) => Memory t a -> Address -> IO a
    loadByte m = readArray (memBytes m) . toMemAddr m
{-# INLINEABLE loadBytes #-}
