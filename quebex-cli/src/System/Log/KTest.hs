-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module System.Log.KTest
  ( LogLevel (..),
    KTestConf (..),
    mkKTestConf,
    writeAssign,
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

data KTestConf
  = KTestConf
  { confLevel :: LogLevel,
    confPath :: FilePath,
    confName :: String
  }
  deriving (Show)

mkKTestConf :: LogLevel -> FilePath -> String -> IO KTestConf
mkKTestConf level directory name = do
  createDirectoryIfMissing True directory
  pure $ KTestConf level directory name

writeAssign :: KTestConf -> LogLevel -> Int -> Assign -> IO ()
writeAssign conf level pathID assign
  | level >= confLevel conf = writeKTest conf pathID (fromAssign assign)
  | otherwise = pure ()

writeKTest :: KTestConf -> Int -> [KTestObj] -> IO ()
writeKTest KTestConf {confPath = directory, confName = name} pathID =
  writeKTest' pathID . KTest [fromString name]
  where
    writeKTest' :: Int -> KTest -> IO ()
    writeKTest' n ktest = do
      flip encodeFile ktest $
        addExtension
          (directory </> ("test" ++ printf "%06d" n))
          ".ktest"
