-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Simulator (simTests) where

import Data.List (find)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Error
import Language.QBE.Simulator.Tracer as T
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

parseAndExec' :: QBE.GlobalIdent -> [D.RegVal] -> String -> IO (Either EvalError (Maybe D.RegVal))
parseAndExec' funcName params input = do
  prog <- case parse "" input of
    Left e -> fail $ "Unexpected parsing error: " ++ show e
    Right r -> pure r

  func <- case find (\f -> QBE.fName f == funcName) (globalFuncs prog) of
    Just x -> pure x
    Nothing -> fail $ "Unknown function: " ++ show funcName

  runExec prog (execFunc func params) T.NoOp

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
      testCase "Invalid subtyping with subword function parameters" $
        do
          res <-
            parseAndExec'
              (QBE.GlobalIdent "subword")
              [D.VByte 0xff]
              "function $subword(ub %val) {\n\
              \@start\n\
              \%val =w add %val, 1\n\
              \ret\n\
              \}"

          res @?= Left TypingError,
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
      testCase "Arithemtics with single-precision float" $
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
      testCase "Arithemtics with double-precision float" $
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
      testCase "Arithemtics with float literal" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "addFloatAndLit")
              [D.VSingle 4.2]
              "function s $addFloatAndLit(s %f) {\n\
              \@start\n\
              \%v =s add %f, 1\n\
              \ret %v\n\
              \}"

          res @?= Just (D.VSingle 5.2),
      testCase "Invalid mixed float arithmetics" $
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
      -- XXX: Doesn't work yet because we are not loading $data into memory.
      -- testCase "Load data object from memory" $
      --   do
      --     res <-
      --       parseAndExec
      --         (QBE.GlobalIdent "main")
      --         []
      --         "data $a = { w 2342 2 3, b 0 }\n\
      --         \function w $main() {\n\
      --         \@start\n\
      --         \%w =w loadw $a\n\
      --         \ret %w\n\
      --         \}"
      --
      --     res @?= Just (D.VWord 2342),
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

          res @?= Just (D.VWord 4)
    ]

simTests :: TestTree
simTests = testGroup "Tests for the Simulator" [blockTests]
