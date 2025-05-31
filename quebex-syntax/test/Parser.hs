module Parser where

import Language.QBE
import Language.QBE.Parser (dataDef, funcDef)
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
      testCase "Data definition with newlines as spaces" $
        let v = DataDef [] "foo" Nothing [OZeroFill 42]
         in parse "data\n$foo={z\n42}" @?= Right v,
      testCase "Data definition with comments" $
        let v = DataDef [] "foo" Nothing [OZeroFill 42]
         in parse "data\n#test\n$foo={z\n#foo\n42}" @?= Right v,
      testCase "Data definition with comments and whitespaces" $
        let v = DataDef [] "foo" Nothing [OZeroFill 42]
         in parse "data\n#test1  \n  #test2\n$foo={z\n#foo\n42}" @?= Right v,
      testCase "Data definition with linkage" $
        let v = DataDef [LExport] "foo" Nothing [OZeroFill 42]
         in parse "export data $foo = { z 42 }" @?= Right v,
      testCase "Data definition with linkage, newlines, and comments" $
        let v = DataDef [LExport, LThread] "foo" Nothing [OZeroFill 42]
         in parse "export\nthread\n#foo\ndata $foo = { z 42 }" @?= Right v,
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

funcTests :: TestTree
funcTests =
  testGroup
    "Function Definition"
    [ testCase "Minimal function definition" $
        let p = [Regular (ABase Word) "argc"]
            b = [Block {label = "start", stmt = [], term = Return Nothing}]
            f = FuncDef [] "main" Nothing p b
         in parse "function $main(w %argc) {\n@start\nret\n}" @?= Right f,
      testCase "Function definition with linkage and return type" $
        let p = [Regular (ABase Long) "v"]
            b = [Block {label = "start", stmt = [], term = Return Nothing}]
            f = FuncDef [LExport, LThread] "example" (Just (ABase Word)) p b
         in parse "export\nthread function w $example(l %v) {\n@start\nret\n}" @?= Right f
    ]
  where
    parse :: String -> Either P.ParseError FuncDef
    parse = P.parse funcDef ""

mkParser :: TestTree
mkParser =
  testGroup
    "Tests for the QBE parser"
    [dataTests, funcTests]
