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
          let b = Block {label = (BlockIdent "start"), stmt = [i], term = Halt}

          res <- runExec (pushStackFrame funcDef >> execBlock b)
          assertEqual "" (envVars <$> res) $ Right (Map.fromList [((LocalIdent "val"), E.EWord 3)]),
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
          let b = Block {label = (BlockIdent "calc"), stmt = [i1, i2], term = Halt}

          res <- runExec (pushStackFrame funcDef >> execBlock b)
          assertEqual "" (envVars <$> res) $ Right (Map.fromList [((LocalIdent "val"), E.EWord 3), ((LocalIdent "foo"), E.EWord 5)]),
      testCase "Alloc pre-aligned value on stack" $
        do
          let i = Assign (LocalIdent "ptr") Long (Alloc AlignWord 4)
          let b = Block {label = (BlockIdent "allocate"), stmt = [i], term = Halt}

          res <- runExec (pushStackFrame funcDef >> execBlock b)
          assertEqual "" (envVars <$> res) $ Right (Map.fromList [((LocalIdent "ptr"), E.ELong 120)]),
      testCase "Store value on the stack" $
        do
          let i1 = Assign (LocalIdent "ptr") Long (Alloc AlignWord 4)
          let i2 = Volatile $ Store (Base Word) (VConst (Const (Number 0x42))) (VLocal (LocalIdent "ptr"))
          let bl = Block {label = (BlockIdent "allocate"), stmt = [i1, i2], term = Halt}

          env <- runExec (pushStackFrame funcDef >> execBlock bl)
          let (Right mem) = envMem <$> env

          res <- MEM.loadByteString mem 120 4
          assertEqual "" res $ BSL.pack [0x42, 0, 0, 0]
    ]
  where
    funcDef :: FuncDef
    funcDef = FuncDef [] (GlobalIdent "foo") Nothing [] []

    envVars e = stkVars (head $ envStk e)

simTests :: TestTree
simTests = testGroup "Tests for the Simulator" [blockTests]
