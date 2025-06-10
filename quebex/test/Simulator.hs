module Simulator (simTests) where

import Control.Monad.State (get)
import Data.Map qualified as Map
import Language.QBE.Parser (funcDef)
import Language.QBE.Simulator
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.State
import Language.QBE.Types
import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.Parsec qualified as P

parseAndExec :: String -> IO BlockResult
parseAndExec func = do
  fDef <- case P.parse funcDef "execFunc" func of
    Left e -> fail $ "Unexpected parsing error: " ++ show e
    Right r -> pure r

  evalRes <- runExec (execFunc fDef)
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
              "function w $addNumbers() {\n\
              \@start\n\
              \%c =w add 1, 2\n\
              \ret %c\n\
              \}\n"

          res @?= Left (Just $ E.VWord 3),
      testCase "Evaluate single basic block with multiple instructions" $
        do
          res <-
            parseAndExec
              "function w $addMultiple() {\n\
              \@begin\n\
              \%val =w add 1, 2\n\
              \%foo =w add %val, 2\n\
              \ret %foo\n\
              \}\n"

          res @?= Left (Just $ E.VWord 5),
      testCase "Evaluate expression with subtyping" $
        do
          res <-
            -- 16045690984835251117 == 0xdeadbeefdecafbad
            parseAndExec
              "function w $subtyping() {\n\
              \@go\n\
              \%val =l add 16045690984835251117, 0\n\
              \%foo =w add %val, 0\n\
              \ret %foo\n\
              \}\n"

          res @?= Left (Just $ E.VWord 0xdecafbad),
      testCase "Evaluate function without return value" $
        do
          res <-
            parseAndExec
              "function $noRet() {\n\
              \@start\n\
              \ret\n\
              \}\n"

          res @?= Left Nothing,
      testCase "Evaluate two basic blocks with unconditional jump" $
        do
          res <-
            parseAndExec
              "function w $unconditionalJump() {\n\
              \@start\n\
              \%val =w add 0, 1\n\
              \jmp @next\n\
              \@next\n\
              \%val =w add %val, 1\n\
              \ret %val\n\
              \}\n"

          res @?= Left (Just $ E.VWord 2),
      testCase "Conditional jump with zero value" $
        do
          res <-
            parseAndExec
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
              \}\n"

          res @?= Left (Just $ E.VLong 23),
      testCase "Conditional jump with non-zero value" $
        do
          res <-
            parseAndExec
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
              \}\n"

          res @?= Left (Just $ E.VLong 42),
      -- TODO: Rewrite once we have load instruction support.
      testCase "Alloc pre-aligned value on stack" $
        do
          let i = Assign (LocalIdent "ptr") Long (Alloc AlignWord 4)
          let b = Block {label = BlockIdent "allocate", stmt = [i], term = Return Nothing}

          res <- runExec (execFunc (makeFunc [b]) >> get)
          assertEqual "" (envVars <$> res) $ Right (Map.fromList [(LocalIdent "ptr", E.VLong 120)]),
      -- TODO: Rewrite once we have load instruction support.
      testCase "Store value on the stack" $
        do
          let i1 = Assign (LocalIdent "ptr") Long (Alloc AlignWord 4)
          let i2 = Volatile $ Store (Base Word) (VConst (Const (Number 0x42))) (VLocal (LocalIdent "ptr"))
          let bl = Block {label = BlockIdent "allocate", stmt = [i1, i2], term = Return Nothing}

          env <- runExec (execFunc (makeFunc [bl]) >> get)
          let (Right mem) = envMem <$> env

          res <- MEM.loadBytes mem 120 4
          assertEqual "" res [0x42, 0, 0, 0]
    ]
  where
    makeFunc :: [Block] -> FuncDef
    makeFunc = FuncDef [] (GlobalIdent "foo") Nothing []

    envVars e = stkVars (head $ envStk e)

simTests :: TestTree
simTests = testGroup "Tests for the Simulator" [blockTests]
