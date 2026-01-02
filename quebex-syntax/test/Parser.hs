-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Parser where

import qualified Data.Map as Map
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
        let v = TypeDef (UserIdent "opaque") (Just 16) (AOpaque 32)
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
         in parse "type :un9 = { { l, s } { w, l } }" @?= Right v,
      testCase "Type definition with trailing comma" $
        let f = [(SExtType (Base Single), Nothing), (SExtType (Base Single), Nothing)]
            v = TypeDef (UserIdent "twofloats") Nothing (ARegular f)
         in parse "type :twofloats = { s, s, }" @?= Right v
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
        let v = DataDef [LExport] (GlobalIdent "b") (Just 8) [OZeroFill 1000]
         in parse "export data $b = align 8 { z 1000 }" @?= Right v,
      testCase "Data definition with linkage section and string escape sequences" $
        let v = DataDef [LSection "f\\oo\\\"bar" Nothing] (GlobalIdent "b") (Just 8) [OZeroFill 1]
         in parse "section \"f\\oo\\\"bar\" data $b =align 8 {z 1}" @?= Right v,
      testCase "Data definition with symbol offset" $
        let v = DataDef {linkage = [], name = GlobalIdent "b", align = Just 8, objs = [OItem (Base Long) [DSymOff (GlobalIdent "s") 1]]}
         in parse "data $b = align 8 { l $s + 1 }" @?= Right v,
      testCase "Data definition with symbol offset and without whitespaces" $
        let v = DataDef {linkage = [], name = GlobalIdent "b", align = Just 8, objs = [OItem (Base Long) [DSymOff (GlobalIdent "s") 1]]}
         in parse "data $b = align 8 {l $s+1}" @?= Right v,
      testCase "Data definition with symbol but without offset" $
        let v = DataDef {linkage = [], name = GlobalIdent "b", align = Just 8, objs = [OItem (Base Long) [DConst (Global (GlobalIdent "s"))]]}
         in parse "data $b = align 8 {l $s}" @?= Right v,
      testCase "Data definition with octal character sequence" $
        let v = DataDef {linkage = [], name = GlobalIdent "b", align = Just 1, objs = [OItem Byte [DString "f\too\NUL"]]}
         in parse "data $b = align 1 { b \"f\\011oo\\000\" }" @?= Right v,
      testCase "Data definition with trailing comma" $
        let v = DataDef {linkage = [], name = GlobalIdent "b", align = Just 1, objs = [OItem Byte [DConst (Number 1)], OItem Byte [DConst (Number 2)]]}
         in parse "data $b = align 1 { b 1, b 2,}" @?= Right v
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
            b = [Block {label = BlockIdent "start", phi = [], stmt = [], term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "main") Nothing p b
         in parse "function $main(w %argc) {\n@start\nret\n}" @?= Right f,
      testCase "Function definition with load instruction" $
        let s = [Assign (LocalIdent "v") Word (Load (LBase Word) (VLocal $ LocalIdent "addr"))]
            b = [Block {label = BlockIdent "begin", phi = [], stmt = s, term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "main") Nothing [] b
         in parse "function $main() {\n@begin\n%v =w loadw %addr\nret\n}" @?= Right f,
      testCase "Function definition with linkage and return type" $
        let p = [Regular (ABase Long) (LocalIdent "v")]
            b = [Block {label = BlockIdent "start", phi = [], stmt = [], term = Return Nothing}]
            f = FuncDef [LExport, LThread] (GlobalIdent "example") (Just (ABase Word)) p b
         in parse "export\nthread function w $example(l %v) {\n@start\nret\n}" @?= Right f,
      testCase "Function definition with section linkage" $
        let p = [Regular (ABase Long) (LocalIdent "v")]
            b = [Block {label = BlockIdent "start", phi = [], stmt = [], term = Return Nothing}]
            f = FuncDef [LSection "foo" Nothing] (GlobalIdent "bla") (Just (ABase Word)) p b
         in parse "section \"foo\"\nfunction w $bla(l %v) {\n@start\nret\n}" @?= Right f,
      testCase "Function definition with subword return type" $
        let b = [Block {label = BlockIdent "here", phi = [], stmt = [], term = Halt}]
            f = FuncDef [] (GlobalIdent "f") (Just (ASubWordType SignedHalf)) [] b
         in parse "function sh $f() {\n@here\nhlt\n}" @?= Right f,
      testCase "Function definition with comments" $
        let p = [Regular (ABase Long) (LocalIdent "v")]
            b = [Block {label = BlockIdent "start", phi = [], stmt = [], term = Return Nothing}]
            f = FuncDef [LSection "foo" (Just "bar")] (GlobalIdent "bla") (Just (ABase Word)) p b
         in parse "section \"foo\" \"bar\"\n#test\nfunction w $bla(l %v) {\n#foo\n@start\n# bar \nret\n#bllubbb\n#bllaaa\n}" @?= Right f,
      testCase "Function definition with comparison instruction" $
        let c = Compare Word CSlt (VConst (Const (Number 23))) (VConst (Const (Number 42)))
            b = [Block {label = BlockIdent "start", phi = [], stmt = [Assign (LocalIdent "res") Word c], term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "f") Nothing [] b
         in parse "function $f() {\n@start\n%res =w csltw 23, 42\nret\n}" @?= Right f,
      testCase "Function definition with extend instruction" $
        let c = Ext SLSignedWord (VConst (Const (Number 42)))
            b = [Block {label = BlockIdent "start", phi = [], stmt = [Assign (LocalIdent "res") Word c], term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "f") Nothing [] b
         in parse "function $f() {\n@start\n%res =w extsw 42\nret\n}" @?= Right f,
      testCase "Function definition with fallthrough block" $
        let b1 = Block {label = BlockIdent "b1", phi = [], stmt = [], term = Jump (BlockIdent "b2")}
            b2 = Block {label = BlockIdent "b2", phi = [], stmt = [], term = Return Nothing}
            f = FuncDef [] (GlobalIdent "f") Nothing [] [b1, b2]
         in parse "function $f() {\n@b1\n@b2\nret\n}" @?= Right f,
      testCase "Block with phi instrunction" $
        let v1 = VConst (Const (Number 1))
            v2 = VConst (Const (Number 2))
            p1 = Phi (LocalIdent "v") Word $ Map.fromList [(BlockIdent "b1", v1), (BlockIdent "b2", v2)]
            b1 = Block {label = BlockIdent "b1", phi = [], stmt = [], term = Jump (BlockIdent "b2")}
            b2 = Block {label = BlockIdent "b2", phi = [], stmt = [], term = Jump (BlockIdent "b3")}
            b3 = Block {label = BlockIdent "b3", phi = [p1], stmt = [], term = Return Nothing}
            fn = FuncDef [] (GlobalIdent "f") Nothing [] [b1, b2, b3]
         in parse "function $f() {\n@b1\njmp @b2\n@b2\njmp @b3\n@b3\n%v =w phi @b1 1, @b2 2\nret\n}" @?= Right fn,
      testCase "Call instruction with integer literal value" $
        let c = Call Nothing (VConst (Const $ Global (GlobalIdent "foo"))) [ArgReg (ABase Word) (VConst (Const (Number 42)))]
            b = [Block {label = BlockIdent "s", phi = [], stmt = [c], term = Return Nothing}]
            f = FuncDef [] (GlobalIdent "f") Nothing [] b
         in parse "function $f() {\n@s\ncall $foo(w 42)\nret\n}" @?= Right f,
      testCase "Unary neg instruction" $
        let i1 = Assign (LocalIdent "r") Word $ Neg (VConst (Const (Number 1)))
            i2 = Assign (LocalIdent "r") Word $ Neg (VLocal $ LocalIdent "r")
            b = Block {label = BlockIdent "s", phi = [], stmt = [i1, i2], term = Halt}
            f = FuncDef [] (GlobalIdent "f") Nothing [] [b]
         in parse "function $f() {\n@s\n%r =w neg 1\n%r =w neg %r\nhlt\n}" @?= Right f,
      testCase "cast instruction" $
        let c = Assign (LocalIdent "r") Word $ Cast (VLocal $ LocalIdent "f")
            b = Block {label = BlockIdent "s", phi = [], stmt = [c], term = Halt}
            f = FuncDef [] (GlobalIdent "f") Nothing [Regular (ABase Single) (LocalIdent "f")] [b]
         in parse "function $f(s %f) {\n@s\n%r =w cast %f\nhlt\n}" @?= Right f,
      testCase "trunc instruction" $
        let c = Assign (LocalIdent "r") Single $ TruncDouble (VLocal $ LocalIdent "d")
            b = Block {label = BlockIdent "s", phi = [], stmt = [c], term = Halt}
            f = FuncDef [] (GlobalIdent "f") Nothing [Regular (ABase Double) (LocalIdent "d")] [b]
         in parse "function $f(d %d) {\n@s\n%r =s truncd %d\nhlt\n}" @?= Right f,
      testCase "exts instruction" $
        let c = Assign (LocalIdent "d") Double $ ExtSingle (VLocal $ LocalIdent "s")
            b = Block {label = BlockIdent "s", phi = [], stmt = [c], term = Halt}
            f = FuncDef [] (GlobalIdent "f") Nothing [Regular (ABase Single) (LocalIdent "s")] [b]
         in parse "function $f(s %s) {\n@s\n%d =d exts %s\nhlt\n}" @?= Right f
    ]
  where
    parse :: String -> Either P.ParseError FuncDef
    parse = P.parse funcDef ""

mkParser :: TestTree
mkParser =
  testGroup
    "Tests for the QBE parser"
    [typeTests, dataTests, funcTests]
