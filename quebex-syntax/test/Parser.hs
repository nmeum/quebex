module Parser where

import Language.QBE
import Language.QBE.Parser (dataDef)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.ParserCombinators.Parsec as P

dataTests :: TestTree
dataTests =
  testGroup
    "Data Definition"
    [ testCase "Data definition with zero fill" $
        let v = DataDef [] "foo" Nothing [OZeroFill 42]
         in parse "data $foo = { z 42 }" @?= Right v,
      testCase "Data definition without optional spaces" $
        let v = DataDef [] "foo" Nothing [OZeroFill 42]
         in parse "data $foo={z 42}" @?= Right v,
      testCase "Data definition with types" $
        let w = [DConst (Number 23), DConst (Number 42)]
            v = DataDef [] "bar" Nothing [OItem (Base Word) w]
         in parse "data $bar = {   w   23   42 }" @?= Right v,
      testCase "An object containing two 64-bit fields" $
        let o =
              [ OItem (Base Long) [DConst (Number 0xffffffffffffffff)],
                OItem (Base Long) [DConst (Number 23)]
              ]
            v = DataDef [] "c" Nothing o
         in parse "data $c = { l -1, l 23 }" @?= Right v
    ]
  where
    parse :: String -> Either P.ParseError DataDef
    parse = P.parse dataDef ""

mkParser :: TestTree
mkParser =
  testGroup
    "Tests for the QBE parser"
    [dataTests]
