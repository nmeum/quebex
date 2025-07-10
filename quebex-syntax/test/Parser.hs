-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Parser where

import Language.QBE.Parser (dataDef, funcDef, typeDef)
import Language.QBE.Types
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.ParserCombinators.Parsec as P

typeTests :: TestTree
typeTests =
  testGroup
    "Aggregate Type Definition"
    [ testCase "Opaque type with alignment" $
        let v = TypeDef (UserIdent "opaque") (Just AlignLongLong) (AOpaque 32)
         in parse "type :opaque = align 16 { 32 }" @?= Right v,
      testCase "Regular empty type" $
        let v = TypeDef (UserIdent "empty") Nothing (ARegular [])
         in parse "type :empty = {}" @?= Right v,
      testCase "Regular type with multiple fields" $
        let f = [(SExtType (Base Single), Nothing), (SExtType (Base Single), Nothing)]
            v = TypeDef (UserIdent "twofloats") Nothing (ARegular f)
         in parse "type :twofloats = { s, s }" @?= Right v,
      testCase "Regular type with trailing whitespaces" $
        let f = [(SExtType Byte, Nothing), (SExtType (Base Word), Just 100)]
            v = TypeDef (UserIdent "abyteandmanywords") Nothing (ARegular f)
         in parse "type :abyteandmanywords = { b, w 100 }" @?= Right v,
      testCase "Union type with multiple fields" $
        let f = [[(SExtType Byte, Nothing)], [(SExtType (Base Single), Nothing)]]
            v = TypeDef (UserIdent "un9") Nothing (AUnion f)
         in parse "type :un9 = { { b } { s } }" @?= Right v,
      testCase "Union type with multiple nested fields" $
        let f =
              [ [(SExtType (Base Long), Nothing), (SExtType (Base Single), Nothing)],
                [(SExtType (Base Word), Nothing), (SExtType (Base Long), Nothing)]
              ]
            v = TypeDef (UserIdent "un9") Nothing (AUnion f)
         in parse "type :un9 = { { l, s } { w, l } }" @?= Right v
    ]
  where
    parse :: String -> Either P.ParseError TypeDef
    parse = P.parse typeDef ""

dataTests :: TestTree
dataTests =
  testGroup
    "Data Definition"
    [ testCase "Data definition with zero fill" $
        let v = DataDef [] (GlobalIdent "foo") Nothing [OZeroFill 42]
         in parse "data $foo = { z 42 }" @?= Right v,
      testCase "Data definition with empty value" $
        let v = DataDef [] (GlobalIdent "foo") Nothing []
         in parse "data $foo = {}" @?= Right v,
      testCase "Data definition without optional spaces" $
        let v = DataDef [] (GlobalIdent "foo") Nothing [OZeroFill 42]
         in parse "data $foo={z 42}" @?= Right v,
      testCase "Data definition with newlines as spaces" $
        let v = DataDef [] (GlobalIdent "foo") Nothing [OZeroFill 42]
         in parse "data\n$foo={z\n42}" @?= Right v,
      testCase "Data definition with comments" $
        let v = DataDef [] (GlobalIdent "foo") Nothing [OZeroFill 42]
         in parse "data\n#test\n$foo={z\n#foo\n42}" @?= Right v,
      testCase "Data definition with comments and whitespaces" $
        let v = DataDef [] (GlobalIdent "foo") Nothing [OZeroFill 42]
         in parse "data\n#test1  \n  #test2\n$foo={z\n#foo\n42}" @?= Right v,
      testCase "Data definition with linkage" $
        let v = DataDef [LExport] (GlobalIdent "foo") Nothing [OZeroFill 42]
         in parse "export data $foo = { z 42 }" @?= Right v,
      testCase "Data definition with linkage, newlines, and comments" $
        let v = DataDef [LExport, LThread] (GlobalIdent "foo") Nothing [OZeroFill 42]
         in parse "export\nthread\n#foo\ndata $foo = { z 42 }" @?= Right v,
      testCase "Data definition with types" $
        let w = [DConst (Number 23), DConst (Number 42)]
            v = DataDef [] (GlobalIdent "bar") Nothing [OItem (Base Word) w]
         in parse "data $bar = {   w   23   42 }" @?= Right v,
      testCase "An object containing two 64-bit fields" $
        let o =
              [ OItem (Base Long) [DConst (Number 0xffffffffffffffff)],
                OItem (Base Long) [DConst (Number 23)]
              ]
            v = DataDef [] (GlobalIdent "c") Nothing o
         in parse "data $c = { l -1, l 23 }" @?= Right v,
      testCase "Data definition with specified alignment and linkage" $
        let v = DataDef [LExport] (GlobalIdent "b") (Just AlignLong) [OZeroFill 1000]
         in parse "export data $b = align 8 { z 1000 }" @?= Right v,
      testCase "Data definition with linkage section and string escape sequences" $
        let v = DataDef [LSection "f\\oo\\\"bar" Nothing] (GlobalIdent "b") (Just AlignLong) [OZeroFill 1]
         in parse "section \"f\\oo\\\"bar\" data $b =align 8 {z 1}" @?= Right v
    ]
  where
    parse :: String -> Either P.ParseError DataDef
    parse = P.parse dataDef ""

funcTests :: TestTree
funcTests =
  testGroup
    "Function Definition"
    [ testCase "Minimal function definition" $
        let p = [Regular (ABase Word) (LocalIdent "argc")]
            b = [Block {label = BlockIdent "start", stmt = [], term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "main") Nothing p b
         in parse "function $main(w %argc) {\n@start\nret\n}" @?= Right f,
      testCase "Function definition with load instruction" $
        let s = [Assign (LocalIdent "v") Word (Load (LBase Word) (VLocal $ LocalIdent "addr"))]
            b = [Block {label = BlockIdent "begin", stmt = s, term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "main") Nothing [] b
         in parse "function $main() {\n@begin\n%v =w loadw %addr\nret\n}" @?= Right f,
      testCase "Function definition with linkage and return type" $
        let p = [Regular (ABase Long) (LocalIdent "v")]
            b = [Block {label = BlockIdent "start", stmt = [], term = Return Nothing}]
            f = FuncDef [LExport, LThread] (GlobalIdent "example") (Just (ABase Word)) p b
         in parse "export\nthread function w $example(l %v) {\n@start\nret\n}" @?= Right f,
      testCase "Function definition with section linkage" $
        let p = [Regular (ABase Long) (LocalIdent "v")]
            b = [Block {label = BlockIdent "start", stmt = [], term = Return Nothing}]
            f = FuncDef [LSection "foo" Nothing] (GlobalIdent "bla") (Just (ABase Word)) p b
         in parse "section \"foo\"\nfunction w $bla(l %v) {\n@start\nret\n}" @?= Right f,
      testCase "Function definition with subword return type" $
        let b = [Block {label = BlockIdent "here", stmt = [], term = Halt}]
            f = FuncDef [] (GlobalIdent "f") (Just (ASubWordType SignedHalf)) [] b
         in parse "function sh $f() {\n@here\nhlt\n}" @?= Right f,
      testCase "Function definition with comments" $
        let p = [Regular (ABase Long) (LocalIdent "v")]
            b = [Block {label = BlockIdent "start", stmt = [], term = Return Nothing}]
            f = FuncDef [LSection "foo" (Just "bar")] (GlobalIdent "bla") (Just (ABase Word)) p b
         in parse "section \"foo\" \"bar\"\n#test\nfunction w $bla(l %v) {\n#foo\n@start\n# bar \nret\n#bllubbb\n#bllaaa\n}" @?= Right f,
      testCase "Function definition with comparison instruction" $
        let c = Compare Word CSlt (VConst (Const (Number 23))) (VConst (Const (Number 42)))
            b = [Block {label = BlockIdent "start", stmt = [Assign (LocalIdent "res") Word c], term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "f") Nothing [] b
         in parse "function $f() {\n@start\n%res =w csltw 23, 42\nret\n}" @?= Right f,
      testCase "Function definition with extend instruction" $
        let c = Ext SLSignedWord (VConst (Const (Number 42)))
            b = [Block {label = BlockIdent "start", stmt = [Assign (LocalIdent "res") Word c], term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "f") Nothing [] b
         in parse "function $f() {\n@start\n%res =w extsw 42\nret\n}" @?= Right f,
      testCase "Call instruction with integer literal value" $
        let c = Call Nothing (VConst $ (Const $ Global (GlobalIdent "foo"))) [ArgReg (ABase Word) (VConst (Const (Number 42)))]
            b = [Block {label = BlockIdent "s", stmt = [c], term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "f") Nothing [] b
         in parse "function $f() {\n@s\ncall $foo(w 42)\nret\n}" @?= Right f
    ]
  where
    parse :: String -> Either P.ParseError FuncDef
    parse = P.parse funcDef ""

mkParser :: TestTree
mkParser =
  testGroup
    "Tests for the QBE parser"
    [typeTests, dataTests, funcTests]
