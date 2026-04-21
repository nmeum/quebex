-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module System.Log.KTest
  ( LogLevel (..),
    LogConf (..),
    mkLogger,
    logAssign,
  )
where

import Data.Binary (encodeFile)
import Data.KTest (KTest (KTest), KTestObj, fromAssign)
import Data.String (fromString)
import Language.QBE.Backend.Store (Assign)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (addExtension, (</>))
import Text.Printf (printf)

data LogLevel = LogAll | LogErr
  deriving (Show, Eq, Ord)

data LogConf
  = LogConf
  { confLevel :: LogLevel,
    confPath :: FilePath,
    confName :: String
  }
  deriving (Show)

mkLogger :: LogLevel -> FilePath -> String -> IO LogConf
mkLogger level directory name = do
  createDirectoryIfMissing True directory
  pure $ LogConf level directory name

getPath :: LogConf -> Int -> String -> FilePath
getPath conf pathID =
  addExtension (confPath conf </> ("test" ++ printf "%06d" pathID))

logAssign :: LogConf -> LogLevel -> Int -> Assign -> IO ()
logAssign conf level pathID assign
  | level >= confLevel conf = writeKTest conf pathID (fromAssign assign)
  | otherwise = pure ()

writeKTest :: LogConf -> Int -> [KTestObj] -> IO ()
writeKTest conf@(LogConf {confName = name}) pathID =
  writeKTest' . KTest [fromString name]
  where
    writeKTest' :: KTest -> IO ()
    writeKTest' ktest = do
      flip encodeFile ktest $
        getPath conf pathID ".ktest"
