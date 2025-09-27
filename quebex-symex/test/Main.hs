-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import BV (bvTests)
import Backend (backendTests)
import Concolic qualified as CE
import Explorer (exploreTests)
import Golden (goldenTests)
import Symbolic qualified as SE
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ SE.exprTests,
      CE.exprTests,
      backendTests,
      exploreTests,
      goldenTests,
      bvTests
    ]
