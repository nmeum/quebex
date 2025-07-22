-- SPDX-FileCopyrightText: 2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.Backend.ExecTree
  ( BTree (..),
    ExecTree,
    mkTree,
    addTrace,
  )
where

import Language.QBE.Backend.Tracer (Branch, ExecTrace, fromBranch)

-- A binary tree.
data BTree a = Node a (Maybe (BTree a)) (Maybe (BTree a)) | Leaf
  deriving (Show, Eq)

-- Execution tree for the exeucted software, represented as follows:
--
--                                 a
--                          True  / \  False
--                               b   …
--                              / \
--                             N   L
--
-- where the edges indicate what happens if branch a is true/false.
-- The left edge covers the true path while the right edge covers the
-- false path.
--
-- The Nothing (N) value indicates that a path has not been explored.
-- In the example above the path `[(True, a), (True, b)]` has not been
-- explored. A Leaf (L) node is used to indicate that a path has been
-- explored but we haven't discored additional branches yet. In the
-- example above the deepest path is hence `[(True a), (False, b)]`.
type ExecTree = BTree Branch

-- Returns 'True' if we can continue exploring on this branch node.
-- This is the case if the node is either a 'Leaf' or 'Nothing'.
canCont :: Maybe (BTree a) -> Bool
canCont Nothing = True
canCont (Just Leaf) = True
canCont _ = False

-- Create a new execution tree from a trace.
mkTree :: ExecTrace -> ExecTree
mkTree [] = Leaf
mkTree [(wasTrue, br)]
  | wasTrue = Node br (Just Leaf) Nothing
  | otherwise = Node br Nothing (Just Leaf)
mkTree ((True, br) : xs) = Node br (Just $ mkTree xs) Nothing
mkTree ((False, br) : xs) = Node br Nothing (Just $ mkTree xs)

-- Add a trace to an existing execution tree. The control flow
-- in the trace must match the existing tree. If it diverges,
-- an error is raised.
--
-- This function prefers the branch nodes from the trace in the
-- resulting 'ExecTree', thus allowing updating their metadata via
-- this function.
--
-- Assertion: The branch encode in the Node and the branch encoded in
-- the trace should also be equal, regarding the encoded condition.
addTrace :: ExecTree -> ExecTrace -> ExecTree
addTrace tree [] = tree
-- The trace takes the True branch and we have taken that previously.
--  ↳ Recursively decent on that branch and look at remaining trace.
addTrace (Node br' (Just tb) fb) ((True, br) : xs) =
  Node (fromBranch br' br) (Just $ addTrace tb xs) fb
-- The trace takes the False branch and we have taken that previously.
--  ↳ Recursively decent on that branch and look at remaining trace.
addTrace (Node br' tb (Just fb)) ((False, br) : xs) =
  Node (fromBranch br' br) tb (Just $ addTrace fb xs)
-- If the trace takes the True/False branch and we have not taken that
-- yet (i.e. canCont is True) we insert the trace at that position.
addTrace (Node br' tb fb) ((wasTrue, br) : xs)
  | canCont tb && wasTrue = Node (fromBranch br' br) (Just $ mkTree xs) fb
  | canCont fb && not wasTrue = Node (fromBranch br' br) tb (Just $ mkTree xs)
  | otherwise = error "unreachable"
-- If we encounter a leaf, this part hasn't been explored yet.
-- That is, we can just insert the trace "as is" at this point.
addTrace Leaf trace = mkTree trace
