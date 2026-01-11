-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# LANGUAGE OverloadedStrings #-}

module KTest (ktestTests) where

import Data.Binary (decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.KTest
import Test.Tasty
import Test.Tasty.HUnit

rawDecode :: FilePath -> IO (KTest, BL.ByteString)
rawDecode fp = do
  content <- BL.readFile fp
  pure (decode content, content)

------------------------------------------------------------------------

ktestTests :: TestTree
ktestTests =
  testGroup
    "KTest"
    [ testCase "single-variable.ktest" $ do
        (ktest, content) <- rawDecode "test/testdata/single-variable.ktest"

        let expected =
              KTest
                { ktArgs = ["shift-test.bc"],
                  ktObjs =
                    [ KTestObj
                        { objName = "x",
                          objBytes = "\NUL\NUL\NUL\NUL"
                        }
                    ]
                }

        ktest @?= expected
        encode expected @?= content,
      testCase "multiple-variables.ktest" $ do
        (ktest, content) <- rawDecode "test/testdata/multiple-variables.ktest"

        let expected =
              KTest
                { ktArgs = ["main.bc"],
                  ktObjs =
                    [ KTestObj "first" "\NUL\NUL\NUL@",
                      KTestObj "second" "\SOH\NUL\NUL@"
                    ]
                }

        ktest @?= expected
        encode expected @?= content
    ]
