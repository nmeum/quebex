module Language.QBE.Simulator.Memory where

import Data.Array.IO (IOUArray, getBounds, newArray)
import Data.Word (Word64, Word8)

type Address = Word64

type Size = Word64

data Memory = Memory
  { memStart :: Address,
    memBytes :: IOUArray Address Word8
  }

mkMemory :: Address -> Size -> IO Memory
mkMemory startAddr size = do
  ary <- newArray (0, size - 1) 0
  return $ Memory startAddr ary

-- Translate global address to a memory-local address.
toMemAddr :: Memory -> Address -> Address
toMemAddr mem addr = addr - (memStart mem)

-- | Returns the size of the memory in bytes.
memSize :: Memory -> IO Size
memSize = fmap ((+ 1) . snd) . getBounds . memBytes
