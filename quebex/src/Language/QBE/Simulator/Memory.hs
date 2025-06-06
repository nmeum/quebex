module Language.QBE.Simulator.Memory
  ( Address,
    Size,
    Memory (..),
    mkMemory,
    toMemAddr,
    memSize,
    storeByteString,
    loadByteString,
  )
where

import Data.Array.IO
  ( IOUArray,
    getBounds,
    newArray,
    readArray,
    writeArray,
  )
import Data.ByteString.Lazy qualified as BSL
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

-- | Store a single byte in memory.
storeByte :: Memory -> Address -> Word8 -> IO ()
storeByte mem addr = writeArray (memBytes mem) $ toMemAddr mem addr

-- | Load a single byte from memory at the given address.
loadByte :: Memory -> Address -> IO Word8
loadByte mem = readArray (memBytes mem) . toMemAddr mem

storeByteString :: Memory -> Address -> BSL.ByteString -> IO ()
storeByteString mem addr bs =
  mapM_ (\(off, val) -> storeByte mem (addr + off) val) $
    zip [0 ..] $
      BSL.unpack bs

loadByteString :: Memory -> Address -> Size -> IO (BSL.ByteString)
loadByteString mem addr byteSize =
  BSL.pack <$> mapM (\off -> loadByte mem (addr + off)) [0 .. byteSize - 1]
