module Main (main) where

import Concolic qualified as C
import Symbolic qualified as S
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [S.exprTests, C.exprTests]
