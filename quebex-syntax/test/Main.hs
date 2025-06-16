module Main (main) where

import Golden
import Parser
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [mkParser, goldenTests]
