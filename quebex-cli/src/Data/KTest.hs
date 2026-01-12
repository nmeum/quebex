-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Data.KTest
  ( KTest (..),
    KTestObj (..),
    fromAssign,
  )
where

import Control.Monad (forM_, void, when)
import Data.Binary (Binary (get, put))
import Data.Binary.Get (getLazyByteString, getWord32be)
import Data.Binary.Put (putLazyByteString, putWord32be)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Word (Word32)
import Language.QBE.Backend.Store (Assign)
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Memory (toBytes)

newtype KTestString
  = KTestString BL.ByteString
  deriving (Show, Eq)

instance Binary KTestString where
  put (KTestString bs) =
    putWord32be (fromIntegral $ BL.length bs) >> putLazyByteString bs

  get = do
    len <- getWord32be
    str <- getLazyByteString (fromIntegral len)
    pure $ KTestString str

------------------------------------------------------------------------

data KTestObj
  = KTestObj
  { objName :: BL.ByteString,
    objBytes :: BL.ByteString
  }
  deriving (Show, Eq)

instance Binary KTestObj where
  put (KTestObj name bytes) = do
    put (KTestString name)
    put (KTestString bytes)

  get = do
    (KTestString name) <- get
    (KTestString bytes) <- get
    pure $ KTestObj name bytes

fromAssign :: Assign -> [KTestObj]
fromAssign assign = map go $ Map.toList assign
  where
    go :: (String, DE.RegVal) -> KTestObj
    go (name, value) =
      KTestObj (fromString name) (BL.pack $ toBytes value)

------------------------------------------------------------------------

data KTest
  = KTest
  { ktArgs :: [BL.ByteString],
    ktObjs :: [KTestObj]
  }
  deriving (Show, Eq)

header :: BL.ByteString
header = fromString "KTEST"

version :: Word32
version = 3

instance Binary KTest where
  get = do
    hdr <- getLazyByteString (BL.length header)
    when (hdr /= header) $
      fail "invalid ktest header"
    ver <- getWord32be
    when (ver > version) $
      fail "unsupported ktest version"

    numArgs <- getWord32be
    strs <-
      mapM
        ( \_ -> do
            (KTestString s) <- get
            pure s
        )
        [1 .. numArgs]

    when (ver >= 2) $
      -- XXX: Skip symArgvs and symArgvLen for now.
      void (getWord32be >> getWord32be)

    numObjs <- getWord32be
    objs <- mapM (const get) [1 .. numObjs]

    pure $ KTest strs objs

  put (KTest args objs) = do
    putLazyByteString header
    putWord32be version

    putWord32be (fromIntegral $ length args)
    forM_ args (put . KTestString)

    -- XXX: Skip symArgvs and symArgvLen for now.
    putWord32be 0
    putWord32be 0

    putWord32be (fromIntegral $ length objs)
    forM_ objs put
