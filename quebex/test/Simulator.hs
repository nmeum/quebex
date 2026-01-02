-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Simulator (simTests) where

import Control.Monad.Catch (try)
import Data.Int (Int32)
import Data.Word (Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32, double2Float, float2Double)
import Language.QBE (parseAndFind)
import Language.QBE.Simulator
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Default.State (Env, mkEnv, run)
import Language.QBE.Simulator.Error
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

parseAndExec' ::
  QBE.GlobalIdent ->
  [D.RegVal] ->
  String ->
  IO (Either EvalError (Maybe D.RegVal))
parseAndExec' funcName params input = do
  (prog, entry) <- parseAndFind funcName input

  env <- mkEnv prog 0 128 :: IO (Env D.RegVal Word8)
  try $ run env (execFunc entry params)

parseAndExec :: QBE.GlobalIdent -> [D.RegVal] -> String -> IO (Maybe D.RegVal)
parseAndExec funcName params input = do
  evalRes <- parseAndExec' funcName params input
  case evalRes of
    Left e -> fail $ "Unexpected evaluation error: " ++ show e
    Right r -> pure r

------------------------------------------------------------------------

blockTests :: TestTree
blockTests =
  testGroup
    "Evaluation of Basic Blocks"
    [ testCase "Evaluate single basic block with single instruction" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "addNumbers")
              []
              "function w $addNumbers() {\n\
              \@start\n\
              \%c =w add 1, 2\n\
              \ret %c\n\
              \}"

          res @?= Just (D.VWord 3),
      testCase "Evaluate single basic block with multiple instructions" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "addMultiple")
              []
              "function w $addMultiple() {\n\
              \@begin\n\
              \%val =w add 1, 2\n\
              \%foo =w add %val, 2\n\
              \ret %foo\n\
              \}"

          res @?= Just (D.VWord 5),
      testCase "Evaluate expression with subtyping" $
        do
          res <-
            -- 16045690984835251117 == 0xdeadbeefdecafbad
            parseAndExec
              (QBE.GlobalIdent "subtyping")
              []
              "function w $subtyping() {\n\
              \@go\n\
              \%val =l add 16045690984835251117, 0\n\
              \%foo =w add %val, 0\n\
              \ret %foo\n\
              \}"

          res @?= Just (D.VWord 0xdecafbad),
      testCase "Subtyping in function return value" $
        do
          res <-
            -- 16045690984835251117 == 0xdeadbeefdecafbad
            parseAndExec
              (QBE.GlobalIdent "subtyp")
              []
              "function w $subtyp() {\n\
              \@start\n\
              \%v =l add 0, 16045690984835251117\n\
              \ret %v\n\
              \}"

          res @?= Just (D.VWord 0xdecafbad),
      testCase "Evaluate function without return value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "noRet")
              []
              "function $noRet() {\n\
              \@start\n\
              \ret\n\
              \}"

          res @?= Nothing,
      testCase "Evaluate two basic blocks with unconditional jump" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "unconditionalJump")
              []
              "function w $unconditionalJump() {\n\
              \@start\n\
              \%val =w add 0, 1\n\
              \jmp @next\n\
              \@next\n\
              \%val =w add %val, 1\n\
              \ret %val\n\
              \}"

          res @?= Just (D.VWord 2),
      testCase "Evalute basic blocks with fallthrough jump" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "unconditionalJump")
              []
              "function w $unconditionalJump() {\n\
              \@start\n\
              \%val =w add 0, 1\n\
              \@next\n\
              \%val =w add %val, 1\n\
              \ret %val\n\
              \}"

          res @?= Just (D.VWord 2),
      testCase "Conditional jump with zero value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "conditionalJumpTaken")
              []
              "function l $conditionalJumpTaken() {\n\
              \@start\n\
              \%zero =w add 0, 0\n\
              \jnz %zero, @nonZero, @zero\n\
              \@nonZero\n\
              \%val =l add 0, 42\n\
              \ret %val\n\
              \@zero\n\
              \%val =l add 0, 23\n\
              \ret %val\n\
              \}"

          res @?= Just (D.VLong 23),
      testCase "Conditional jump with non-zero value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "conditionalJumpTaken")
              []
              "function l $conditionalJumpTaken() {\n\
              \@start\n\
              \%zero =w add 1, 0\n\
              \jnz %zero, @nonZero, @zero\n\
              \@nonZero\n\
              \%val =l add 0, 42\n\
              \ret %val\n\
              \@zero\n\
              \%val =l add 0, 23\n\
              \ret %val\n\
              \}"

          res @?= Just (D.VLong 42),
      testCase "Execute a function with parameters" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "funcWithParam")
              [D.VWord 41]
              "function w $funcWithParam(w %x) {\n\
              \@go\n\
              \%y =w add 1, %x\n\
              \ret %y\n\
              \}"

          res @?= Just (D.VWord 42),
      testCase "Function call instruction without return value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "function $foo(w %x) {\n\
              \@start\n\
              \%y =w sub 42, 0\n\
              \ret\n\
              \}\n\
              \function w $main() {\n\
              \@start\n\
              \%y =w sub 0, 0\n\
              \call $foo(w %y)\n\
              \ret %y\n\
              \}"

          res @?= Just (D.VWord 0),
      testCase "Function call with return value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "function w $foo(w %x) {\n\
              \@start\n\
              \%y =w sub %x, 19\n\
              \ret %y\n\
              \}\n\
              \function w $main() {\n\
              \@start\n\
              \%x =w add 0, 42\n\
              \%ret =w call $foo(w %x)\n\
              \ret %ret\n\
              \}"

          res @?= Just (D.VWord 23),
      testCase "Allocate, store and load value in memory" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "allocate")
              []
              "function w $allocate() {\n\
              \@start\n\
              \%addr =l alloc4 4\n\
              \storew 2342, %addr\n\
              \%v =w loadw %addr\n\
              \ret %v\n\
              \}"

          res @?= Just (D.VWord 2342),
      testCase "Load with sub word type" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "allocate")
              []
              "function w $allocate() {\n\
              \@start\n\
              \%addr =l alloc4 4\n\
              \storeb 249, %addr\n\
              \%v =w loadsb %addr\n\
              \ret %v\n\
              \}"

          -- 249 (0xf9) sign extended to 32-bit.
          res @?= Just (D.VWord 0xfffffff9),
      testCase "Store subword in memory" $
        do
          res <-
            -- 2863311530 == 0xaaaaaaaa
            parseAndExec
              (QBE.GlobalIdent "storeByte")
              []
              "function w $storeByte() {\n\
              \@start\n\
              \%addr =l alloc4 4\n\
              \storew 2863311530, %addr\n\
              \storeb 255, %addr\n\
              \%v =w loadw %addr\n\
              \ret %v\n\
              \}"

          res @?= Just (D.VWord 0xaaaaaaff),
      testCase "Function with user-defined type as function parameter" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "type :one = { w }\n\
              \function w $getone(:one %ptr) {\n\
              \@start\n\
              \%val =w loadw %ptr\n\
              \ret %val\n\
              \}\n\
              \function w $main() {\n\
              \@entry\n\
              \%addr =l alloc4 4\n\
              \storew 3735928559, %addr\n\
              \%ret =w call $getone(l %addr)\n\
              \ret %ret\n\
              \}"

          res @?= Just (D.VWord 0xdeadbeef),
      testCase "Pointer arithmetic on user-defined type" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "type :abyteandmanywords = { w, b 100 }\n\
              \function w $main() {\n\
              \@start.1\n\
              \%addr.0 =l alloc4 104\n\
              \%addr.1 =l add %addr.0, 4\n\
              \storeb 255, %addr.1\n\
              \%addr.2 =l sub %addr.1, 4\n\
              \storew 3735928304, %addr.2\n\
              \%word =w loaduw %addr.0\n\
              \%byte =w loadub %addr.1\n\
              \%res  =w add %word, %byte\n\
              \ret %res\n\
              \}"

          res @?= Just (D.VWord 0xdeadbeef),
      testCase "Subtyping with subword function parameters" $
        do
          res <-
            parseAndExec'
              (QBE.GlobalIdent "subword")
              [D.VWord 0xff]
              "function $subword(ub %val) {\n\
              \@start\n\
              \%val =w add %val, 1\n\
              \ret\n\
              \}"

          res @?= Right Nothing,
      testCase "Jump to unknown block within function" $
        do
          res <-
            parseAndExec'
              (QBE.GlobalIdent "main")
              []
              "function $main() {\n\
              \@start\n\
              \jmp @foo\n\
              \@bar\n\
              \ret\n\
              \}"

          res @?= Left (UnknownBlock $ QBE.BlockIdent "foo"),
      testCase "Call undefined function" $
        do
          res <-
            parseAndExec'
              (QBE.GlobalIdent "main")
              []
              "function $main() {\n\
              \@start\n\
              \call $bar()\n\
              \ret\n\
              \}"

          res @?= Left (UnknownFunction $ QBE.GlobalIdent "bar"),
      testCase "Arithmetic with single-precision float" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "addFloats")
              [D.VSingle 2.0, D.VSingle 0.3]
              "function s $addFloats(s %f1, s %f2) {\n\
              \@start\n\
              \%val =s add %f1, %f2\n\
              \ret %val\n\
              \}"

          res @?= Just (D.VSingle 2.3),
      testCase "Arithmetic with double-precision float" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "addFloats")
              [D.VDouble 2.0, D.VDouble 0.3]
              "function d $addFloats(d %f1, d %f2) {\n\
              \@start\n\
              \%val =d add %f1, %f2\n\
              \ret %val\n\
              \}"

          res @?= Just (D.VDouble 2.3),
      testCase "Arithmetic with float literal" $
        do
          res1 <-
            parseAndExec
              (QBE.GlobalIdent "addFloatAndLit")
              [D.VSingle 4.2]
              "function s $addFloatAndLit(s %f) {\n\
              \@start\n\
              \%v =s add %f, 1\n\
              \ret %v\n\
              \}"

          -- This returns 4.2, not 5.2 because the 1 is interpreted
          -- as a bitwise representation of an IEEE floating point.
          --
          -- QBE itself also treats it in this way.
          res1 @?= Just (D.VSingle 4.2)

          -- The following works because it uses the single literal.
          res2 <-
            parseAndExec
              (QBE.GlobalIdent "addFloatAndLit")
              [D.VSingle 4.2]
              "function s $addFloatAndLit(s %f) {\n\
              \@start\n\
              \%v =s add %f, s_1.0\n\
              \ret %v\n\
              \}"

          res2 @?= Just (D.VSingle 5.2),
      testCase "Invalid mixed float arithmetic" $
        do
          res <-
            parseAndExec'
              (QBE.GlobalIdent "addFloatAndLong")
              [D.VSingle 4.2, D.VLong 42]
              "function s $addFloatAndLong(s %f, l %l) {\n\
              \@start\n\
              \%v =s add %f, %l\n\
              \ret %v\n\
              \}"

          res @?= Left TypingError,
      testCase "Store float in memory and load it again" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "storeAndLoadFloat")
              [D.VSingle 0.333333333]
              "function s $storeAndLoadFloat(s %f) {\n\
              \@start.1\n\
              \%addr =l alloc4 4\n\
              \stores %f, %addr\n\
              \%loaded =s loads %addr\n\
              \ret %loaded\n\
              \}"

          res @?= Just (D.VSingle 0.333333333),
      testCase "Store double in memory and load it again" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "storeAndLoadDouble")
              [D.VDouble 0.3333333331111]
              "function d $storeAndLoadDouble(d %f) {\n\
              \@start.1\n\
              \%addr =l alloc4 8\n\
              \stored %f, %addr\n\
              \%loaded =d loadd %addr\n\
              \ret %loaded\n\
              \}"

          res @?= Just (D.VDouble 0.3333333331111),
      testCase "Load data object from memory" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $a = { b \"ABCD\" }\n\
              \function w $main() {\n\
              \@start\n\
              \%w =w loadw $a\n\
              \ret %w\n\
              \}"

          res @?= Just (D.VWord 0x44434241),
      testCase "Data definition with symbol reference" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $a = { b \"ABCD\" }\n\
              \data $p = { l $a }\n\
              \function w $main() {\n\
              \@start\n\
              \%ptr =l loadl $p\n\
              \%res =w loadw %ptr\n\
              \ret %res\n\
              \}"

          res @?= Just (D.VWord 0x44434241),
      testCase "Data definition with symbol offset" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $a = align 1 { b \"ABCDE\" }\n\
              \data $p = align 8 { l $a + 1 }\n\
              \function w $main() {\n\
              \@start\n\
              \%ptr =l loadl $p\n\
              \%res =w loadw %ptr\n\
              \ret %res\n\
              \}"

          res @?= Just (D.VWord 0x45444342),
      testCase "Data definition with constant number" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $a = align 4 { l 42 }\n\
              \function l $main() {\n\
              \@start\n\
              \%l =l loadl $a\n\
              \ret %l\n\
              \}"

          res @?= Just (D.VLong 42),
      testCase "Data definition with multiple fields" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $a = align 1 { b \"ABCD\", w 42 }\n\
              \data $p = align 8 { l $a + 4 }\n\
              \function w $main() {\n\
              \@start\n\
              \%ptr =l loadl $p\n\
              \%res =w loadw %ptr\n\
              \ret %res\n\
              \}"

          res @?= Just (D.VWord 42),
      testCase "Data definition with zero fill" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $a = align 1 { w 4294967295, z 4, w 4294967295 }\n\
              \data $p = align 8 { l $a }\n\
              \function w $main() {\n\
              \@start\n\
              \%ptr =l loadl $p\n\
              \%ptr =l add %ptr, 4\n\
              \%res =w loadw %ptr\n\
              \ret %res\n\
              \}"

          res @?= Just (D.VWord 0),
      testCase "Data definition with single" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $a = align 1 { s s_2.3, s s_4.2 }\n\
              \data $p = align 8 { l $a }\n\
              \function s $main() {\n\
              \@start\n\
              \%ptr.1 =l loadl $p\n\
              \%ptr.2 =l add %ptr.1, 4\n\
              \%val.1 =s loads %ptr.1\n\
              \%val.2 =s loads %ptr.2\n\
              \%res =s add %val.1, %val.2\n\
              \ret %res\n\
              \}"
          res @?= Just (D.VSingle $ 2.3 + 4.2),
      testCase "Recursive data definition" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $c = { l -1, l $c }\n\
              \function w $main() {\n\
              \@start\n\
              \%ptr.1 =l add $c, 0\n\
              \%ptr.2 =l add $c, 8\n\
              \%field =l loadl %ptr.2\n\
              \%ptrEq =w ceql %field, %ptr.1\n\
              \ret %ptrEq\n\
              \}"

          res @?= Just (D.VWord 1),
      testCase "Data definition with maximum struct member alignment" $
        do
          res <-
            -- The maximum alignment of a struct member for the struct
            -- referenced by `$ptr` is 8 (the long member). Therefore,
            -- the struct must be allocated on a 8-Byte-aligned address.
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "data $fill = { w 0 }\n\
              \data $ptr = { b 255, w 2342, l 1337, b 255 }\n\
              \function w $main() {\n\
              \@start\n\
              \%p =l urem $ptr, 8\n\
              \%correctAlign =w ceql %p, 0\n\
              \ret %correctAlign\n\
              \}"

          res @?= Just (D.VWord 1),
      testCase "Subtyping with load instruction" $
        do
          res <-
            -- 16045690984835251117 == 0xdeadbeefdecafbad
            parseAndExec
              (QBE.GlobalIdent "allocate")
              []
              "function w $allocate() {\n\
              \@start\n\
              \%addr =l alloc4 4\n\
              \storel 16045690984835251117, %addr\n\
              \%v =w loadl %addr\n\
              \ret %v\n\
              \}"

          res @?= Just (D.VWord 0xdecafbad),
      testCase "Subtyped branch condition" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "condJump")
              []
              "function w $condJump() {\n\
              \@start\n\
              \%zero =l add 0, 0\n\
              \jnz %zero, @nonZero, @zero\n\
              \@nonZero\n\
              \%val =w add 0, 42\n\
              \ret %val\n\
              \@zero\n\
              \%val =w add 0, 23\n\
              \ret %val\n\
              \}"

          res @?= Just (D.VWord 23),
      testCase "Multiple jumps" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "branchOnInput")
              [D.VWord 0, D.VWord 0]
              "function w $branchOnInput(w %cond1, w %cond2) {\n\
              \@jump.1\n\
              \jnz %cond1, @branch.1, @branch.2\n\
              \@branch.1\n\
              \jmp @jump.2\n\
              \@branch.2\n\
              \jmp @jump.2\n\
              \@jump.2\n\
              \jnz %cond2, @branch.3, @branch.4\n\
              \@branch.3\n\
              \ret 3\n\
              \@branch.4\n\
              \ret 4\n\
              \}"

          res @?= Just (D.VWord 4),
      testCase "Blit instruction w/o overlaps" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              [D.VWord 0xdeadbeef]
              "function w $main(w %word) {\n\
              \@start\n\
              \%src =l alloc4 4\n\
              \%dst =l alloc4 4\n\
              \storew %word, %src\n\
              \blit %src, %dst, 4\n\
              \%ret =w loadw %dst\n\
              \ret %ret\n\
              \}"

          res @?= Just (D.VWord 0xdeadbeef),
      testCase "Blit instruction with no bytes to copy" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              [D.VWord 0xdeadbeef]
              "function w $main(w %word) {\n\
              \@start\n\
              \%src =l alloc4 4\n\
              \%dst =l alloc4 4\n\
              \storew %word, %src\n\
              \storew 42, %dst\n\
              \blit %src, %dst, 0\n\
              \%ret =w loadw %dst\n\
              \ret %ret\n\
              \}"

          res @?= Just (D.VWord 42),
      testCase "Comparision instruction" $
        do
          let prog =
                "function w $main(w %lhs, w %rhs) {\n\
                \@start\n\
                \%r =w csltw %lhs, %rhs\n\
                \ret %r\n\
                \}"

          resLarger <-
            parseAndExec
              (QBE.GlobalIdent "main")
              [D.VWord 0, D.VWord 1]
              prog
          resLarger @?= Just (D.VWord 1)

          resSmaller <-
            parseAndExec
              (QBE.GlobalIdent "main")
              [D.VWord 1, D.VWord 0]
              prog
          resSmaller @?= Just (D.VWord 0),
      testCase "Compare with subtyping" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              [D.VWord 1, D.VLong 0]
              "function w $main(w %lhs, l %rhs) {\n\
              \@start\n\
              \%r =w csltw %lhs, %rhs\n\
              \ret %r\n\
              \}"

          res @?= Just (D.VWord 0),
      testCase "Compare with long exceeding 32-bit" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "function w $main() {\n\
              \@start\n\
              \%r =w cultl 4294967296, 20\n\
              \ret %r\n\
              \}"

          res @?= Just (D.VWord 0),
      testCase "Phi instruction" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "f")
              []
              "function w $f() {\n\
              \@begin\n\
              \jmp @start2\n\
              \@start1\n\
              \jmp @phi\n\
              \@start2\n\
              \jmp @phi\n\
              \@phi\n\
              \%v =w phi @start1 23, @start2 42\n\
              \ret %v\n\
              \}"

          res @?= Just (D.VWord 42),
      testCase "Sign extend subword" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "ext")
              [D.VWord 128]
              "function w $ext(w %word) {\n\
              \@start\n\
              \%r =w extsb %word\n\
              \ret %r\n\
              \}"

          res @?= Just (D.VWord 0xffffff80),
      testCase "Zero extend subword" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "ext")
              [D.VWord 128]
              "function w $ext(w %word) {\n\
              \@start\n\
              \%r =w extub %word\n\
              \ret %r\n\
              \}"

          res @?= Just (D.VWord 0x00000080),
      testCase "Shift instructions" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "shift")
              [D.VWord 0xdeadbeef]
              "function w $shift(w %word) {\n\
              \@start\n\
              \%r =w shr 3735928559, 4\n\
              \%r =w shl %r, 4\n\
              \ret %r\n\
              \}"

          res @?= Just (D.VWord 0xdeadbee0),
      testCase "Shift with long amount" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "shift")
              []
              "function l $shift() {\n\
              \@start\n\
              \%r =l shl 2, 4294967300\n\
              \ret %r\n\
              \}"

          -- 4294967300 overflows to 4 so this is: 2 << 4.
          res @?= Just (D.VLong 32),
      testCase "Div instruction with single" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "div")
              []
              "function s $div() {\n\
              \@start\n\
              \%r =s div s_5.0, s_2.0\n\
              \ret %r\n\
              \}"

          res @?= Just (D.VSingle 2.5),
      testCase "Div instruction with double" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "div")
              []
              "function d $div() {\n\
              \@start\n\
              \%r =d div d_5.0, d_2.0\n\
              \ret %r\n\
              \}"

          res @?= Just (D.VDouble 2.5),
      testCase "Div instruction with word" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "div")
              []
              "function w $div() {\n\
              \@start\n\
              \%r =w div 5, 2\n\
              \ret %r\n\
              \}"

          res @?= Just (D.VWord 2),
      testCase "Copy instruction with subtyping" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "function w $main() {\n\
              \@start\n\
              \%l =l copy 42\n\
              \%w =w copy %l\n\
              \ret %w\n\
              \}"

          res @?= Just (D.VWord 42),
      testCase "Cast from single to word" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "function w $main() {\n\
              \@start\n\
              \%s =s add s_0.0, s_4.2\n\
              \%w =w cast %s\n\
              \ret %w\n\
              \}"

          let ftow = castFloatToWord32 4.2
          res @?= Just (D.VWord ftow),
      testCase "Cast from double to long" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              []
              "function l $main() {\n\
              \@start\n\
              \%d =d add d_0.0, d_4.2\n\
              \%l =l cast %d\n\
              \ret %l\n\
              \}"

          let dtol = castDoubleToWord64 4.2
          res @?= Just (D.VLong dtol),
      testCase "Cast from word to single" $
        do
          let ftow = castFloatToWord32 4.2
          res <-
            parseAndExec
              (QBE.GlobalIdent "cast")
              [D.VWord ftow]
              "function s $cast(w %w) {\n\
              \@start\n\
              \%s =s cast %w\n\
              \ret %s\n\
              \}"

          res @?= Just (D.VSingle 4.2),
      testCase "Cast from long to double" $
        do
          let dtol = castDoubleToWord64 4.2342
          res <-
            parseAndExec
              (QBE.GlobalIdent "cast")
              [D.VLong dtol]
              "function d $cast(l %l) {\n\
              \@start\n\
              \%d =d cast %l\n\
              \ret %d\n\
              \}"

          res @?= Just (D.VDouble 4.2342),
      testCase "Trunc double to single" $
        do
          let f = 4.293170199018932489308403284024098032
          res <-
            parseAndExec
              (QBE.GlobalIdent "trunc")
              [D.VDouble f]
              "function s $trunc(d %d) {\n\
              \@start\n\
              \%s =s truncd %d\n\
              \ret %s\n\
              \}"

          let d2f = D.VSingle $ double2Float f
          res @?= Just d2f,
      testCase "Extend float to double" $
        do
          let f = 23.42
          res <-
            parseAndExec
              (QBE.GlobalIdent "ext")
              [D.VSingle f]
              "function d $ext(s %s) {\n\
              \@start\n\
              \%d =d exts %s\n\
              \ret %d\n\
              \}"

          let f2d = D.VDouble $ float2Double f
          res @?= Just f2d,
      testCase "Convert single to unsigned long" $
        do
          let f = 4.2
          res <-
            parseAndExec
              (QBE.GlobalIdent "fcon")
              [D.VSingle f]
              "function l $fcon(s %s) {\n\
              \@start\n\
              \%ret =l stoui %s\n\
              \ret %ret\n\
              \}"

          res @?= Just (D.VLong 4),
      testCase "Convert single to signed word" $
        do
          let f = -3.99
          res <-
            parseAndExec
              (QBE.GlobalIdent "fcon")
              [D.VSingle f]
              "function w $fcon(s %s) {\n\
              \@start\n\
              \%ret =w stosi %s\n\
              \ret %ret\n\
              \}"

          res @?= Just (D.VWord $ fromIntegral (-3 :: Int32)),
      testCase "Convert double to unsigned int" $
        do
          let f = 4.9
          res <-
            parseAndExec
              (QBE.GlobalIdent "fcon")
              [D.VDouble f]
              "function l $fcon(d %d) {\n\
              \@start\n\
              \%ret =l dtoui %d\n\
              \ret %ret\n\
              \}"

          res @?= Just (D.VLong 4)
    ]

simTests :: TestTree
simTests = testGroup "Tests for the Simulator" [blockTests]
