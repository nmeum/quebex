module Simulator (simTests) where

import Data.List (find)
import Data.Map qualified as Map
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.State (RegMap)
import Language.QBE.Types qualified as QBE
import Test.Tasty
import Test.Tasty.HUnit

parseAndExec :: QBE.GlobalIdent -> RegMap -> String -> IO (Maybe E.RegVal)
parseAndExec funcName params input = do
  prog <- case parse "" input of
    Left e -> fail $ "Unexpected parsing error: " ++ show e
    Right r -> pure r

  func <- case find (\f -> QBE.fName f == funcName) (globalFuncs prog) of
    Just x -> pure x
    Nothing -> fail $ "Unknown function: " ++ show funcName

  evalRes <- runExec prog (execFunc func params)
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
              Map.empty
              "function w $addNumbers() {\n\
              \@start\n\
              \%c =w add 1, 2\n\
              \ret %c\n\
              \}"

          res @?= Just (E.VWord 3),
      testCase "Evaluate single basic block with multiple instructions" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "addMultiple")
              Map.empty
              "function w $addMultiple() {\n\
              \@begin\n\
              \%val =w add 1, 2\n\
              \%foo =w add %val, 2\n\
              \ret %foo\n\
              \}"

          res @?= Just (E.VWord 5),
      testCase "Evaluate expression with subtyping" $
        do
          res <-
            -- 16045690984835251117 == 0xdeadbeefdecafbad
            parseAndExec
              (QBE.GlobalIdent "subtyping")
              Map.empty
              "function w $subtyping() {\n\
              \@go\n\
              \%val =l add 16045690984835251117, 0\n\
              \%foo =w add %val, 0\n\
              \ret %foo\n\
              \}"

          res @?= Just (E.VWord 0xdecafbad),
      testCase "Subtyping in function return value" $
        do
          res <-
            -- 16045690984835251117 == 0xdeadbeefdecafbad
            parseAndExec
              (QBE.GlobalIdent "subtyp")
              Map.empty
              "function w $subtyp() {\n\
              \@start\n\
              \%v =l add 0, 16045690984835251117\n\
              \ret %v\n\
              \}"

          res @?= Just (E.VWord 0xdecafbad),
      testCase "Evaluate function without return value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "noRet")
              Map.empty
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
              Map.empty
              "function w $unconditionalJump() {\n\
              \@start\n\
              \%val =w add 0, 1\n\
              \jmp @next\n\
              \@next\n\
              \%val =w add %val, 1\n\
              \ret %val\n\
              \}"

          res @?= Just (E.VWord 2),
      testCase "Conditional jump with zero value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "conditionalJumpTaken")
              Map.empty
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

          res @?= Just (E.VLong 23),
      testCase "Conditional jump with non-zero value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "conditionalJumpTaken")
              Map.empty
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

          res @?= Just (E.VLong 42),
      testCase "Execute a function with parameters" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "funcWithParam")
              (Map.fromList [(QBE.LocalIdent "x", E.VWord 41)])
              "function w $funcWithParam(w %x) {\n\
              \@go\n\
              \%y =w add 1, %x\n\
              \ret %y\n\
              \}"

          res @?= Just (E.VWord 42),
      testCase "Function call instruction without return value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              Map.empty
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

          res @?= Just (E.VWord 0),
      testCase "Function call with return value" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "main")
              Map.empty
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

          res @?= Just (E.VWord 23),
      testCase "Allocate, store and load value in memory" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "allocate")
              Map.empty
              "function w $allocate() {\n\
              \@start\n\
              \%addr =l alloc4 4\n\
              \storew 2342, %addr\n\
              \%v =w loadw %addr\n\
              \ret %v\n\
              \}"

          res @?= Just (E.VWord 2342),
      testCase "Load with sub word type" $
        do
          res <-
            parseAndExec
              (QBE.GlobalIdent "allocate")
              Map.empty
              "function w $allocate() {\n\
              \@start\n\
              \%addr =l alloc4 4\n\
              \storeb 249, %addr\n\
              \%v =w loadsb %addr\n\
              \ret %v\n\
              \}"

          -- 249 (0xf9) sign extended to 32-bit.
          res @?= Just (E.VWord 0xfffffff9),
      testCase "Store subword in memory" $
        do
          res <-
            -- 2863311530 == 0xaaaaaaaa
            parseAndExec
              (QBE.GlobalIdent "storeByte")
              Map.empty
              "function w $storeByte() {\n\
              \@start\n\
              \%addr =l alloc4 4\n\
              \storew 2863311530, %addr\n\
              \storeb 255, %addr\n\
              \%v =w loadw %addr\n\
              \ret %v\n\
              \}"

          res @?= Just (E.VWord 0xaaaaaaff),
      testCase "Subtyping with load instruction" $
        do
          res <-
            -- 16045690984835251117 == 0xdeadbeefdecafbad
            parseAndExec
              (QBE.GlobalIdent "allocate")
              Map.empty
              "function w $allocate() {\n\
              \@start\n\
              \%addr =l alloc4 4\n\
              \storew 16045690984835251117, %addr\n\
              \%v =w loadl %addr\n\
              \ret %v\n\
              \}"

          res @?= Just (E.VWord 0xdecafbad)
    ]

simTests :: TestTree
simTests = testGroup "Tests for the Simulator" [blockTests]
