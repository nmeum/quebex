module Simulator (simTests) where

import Data.Map qualified as Map
import Language.QBE.Simulator
import Language.QBE.Simulator.Expression qualified as E
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

          res <- runExec (execBlock b)
          assertEqual "" res $ Right (Map.fromList [("%val", E.EWord 3)]),
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

          res <- runExec (execBlock b)
          assertEqual "" res $ Right (Map.fromList [("%val", E.EWord 3), ("%foo", E.EWord 5)])
    ]

simTests :: TestTree
simTests = testGroup "Tests for the Simulator" [blockTests]
