module Language.QBE.Simulator.Memory
  ( Address,
    Size,
    Memory (..),
    mkMemory,
    toMemAddr,
    memSize,
    loadBytes,
    storeBytes,
  )
where

import Data.Array.IO
  ( IOUArray,
    getBounds,
    newArray,
    readArray,
    writeArray,
  )
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
toMemAddr mem addr = addr - memStart mem

-- | Returns the size of the memory in bytes.
memSize :: Memory -> IO Size
memSize = fmap ((+ 1) . snd) . getBounds . memBytes

storeBytes :: Memory -> Address -> [Word8] -> IO ()
storeBytes mem addr bytes =
  mapM_ (\(off, val) -> storeByte mem (addr + off) val) $
    zip [0 ..] bytes
  where
    storeByte :: Memory -> Address -> Word8 -> IO ()
    storeByte m a = writeArray (memBytes m) $ toMemAddr mem a

loadBytes :: Memory -> Address -> Size -> IO [Word8]
loadBytes mem addr byteSize =
  mapM (\off -> loadByte mem (addr + off)) [0 .. byteSize - 1]
  where
    loadByte :: Memory -> Address -> IO Word8
    loadByte m = readArray (memBytes m) . toMemAddr m
