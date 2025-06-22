module Main (main) where

import Concolic qualified as CE
import Simulator qualified as SIM
import Symbolic qualified as SE
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [SE.exprTests, CE.exprTests, SIM.simTests]
