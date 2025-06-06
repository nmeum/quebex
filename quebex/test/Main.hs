module Main (main) where

import Memory
import Simulator
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [simTests, memTests]
