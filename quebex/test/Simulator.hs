module Simulator (simTests) where

import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Language.QBE.Simulator
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.State
import Language.QBE.Types
import Test.Tasty
import Test.Tasty.HUnit

blockTests :: TestTree
blockTests =
  testGroup
    "Evaluation of Basic Blocks"
    [ testCase "Evaluate single basic block" $
        do
          let i =
                Assign
                  (LocalIdent "val")
                  Word
                  ( Add
                      (VConst (Const (Number 1)))
                      (VConst (Const (Number 2)))
                  )
          let b = Block {label = BlockIdent "start", stmt = [i], term = Return Nothing}

          res <- runExec (execFunc $ makeFunc [b])
          assertEqual "" (envVars <$> res) $ Right (Map.fromList [(LocalIdent "val", E.VWord 3)]),
      testCase "Evaluate multiple basic blocks" $
        do
          let i1 =
                Assign
                  (LocalIdent "val")
                  Word
                  ( Add
                      (VConst (Const (Number 1)))
                      (VConst (Const (Number 2)))
                  )
          let i2 =
                Assign
                  (LocalIdent "foo")
                  Word
                  ( Add
                      (VLocal (LocalIdent "val"))
                      (VConst (Const (Number 2)))
                  )
          let b = Block {label = BlockIdent "calc", stmt = [i1, i2], term = Return Nothing}

          res <- runExec (execFunc $ makeFunc [b])
          assertEqual "" (envVars <$> res) $ Right (Map.fromList [(LocalIdent "val", E.VWord 3), (LocalIdent "foo", E.VWord 5)]),
      testCase "Evaluate expression with subtyping" $
        do
          let i1 =
                Assign
                  (LocalIdent "val")
                  Long
                  ( Add
                      (VConst (Const (Number 0xdeadbeefdecafbad)))
                      (VConst (Const (Number 0)))
                  )
          let i2 =
                Assign
                  (LocalIdent "foo")
                  Word
                  ( Add
                      (VLocal (LocalIdent "val"))
                      (VConst (Const (Number 0)))
                  )
          let b = Block {label = BlockIdent "subtype", stmt = [i1, i2], term = Return Nothing}

          res <- runExec (execFunc $ makeFunc [b])
          assertEqual "" (envVars <$> res) $ Right (Map.fromList [(LocalIdent "val", E.VLong 0xdeadbeefdecafbad), (LocalIdent "foo", E.VWord 0xdecafbad)]),
      testCase "Alloc pre-aligned value on stack" $
        do
          let i = Assign (LocalIdent "ptr") Long (Alloc AlignWord 4)
          let b = Block {label = BlockIdent "allocate", stmt = [i], term = Return Nothing}

          res <- runExec (execFunc $ makeFunc [b])
          assertEqual "" (envVars <$> res) $ Right (Map.fromList [(LocalIdent "ptr", E.VLong 120)]),
      testCase "Store value on the stack" $
        do
          let i1 = Assign (LocalIdent "ptr") Long (Alloc AlignWord 4)
          let i2 = Volatile $ Store (Base Word) (VConst (Const (Number 0x42))) (VLocal (LocalIdent "ptr"))
          let bl = Block {label = BlockIdent "allocate", stmt = [i1, i2], term = Return Nothing}

          env <- runExec (execFunc $ makeFunc [bl])
          let (Right mem) = envMem <$> env

          res <- MEM.loadByteString mem 120 4
          assertEqual "" res $ BSL.pack [0x42, 0, 0, 0]
    ]
  where
    makeFunc :: [Block] -> FuncDef
    makeFunc = FuncDef [] (GlobalIdent "foo") Nothing []

    envVars e = stkVars (head $ envStk e)

simTests :: TestTree
simTests = testGroup "Tests for the Simulator" [blockTests]
