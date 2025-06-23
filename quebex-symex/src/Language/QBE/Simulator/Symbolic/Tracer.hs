-- SPDX-FileCopyrightText: 2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.Simulator.Symbolic.Tracer
  ( Branch (Branch),
    newBranch,
    fromBranch,
    ExecTrace,
    newExecTrace,
    toSExprs,
    appendBranch,
    appendCons,
  )
where

import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import SimpleSMT qualified as SMT

-- Represents a branch condition in the executed code
data Branch
  = Branch
      Bool -- Whether negation of the branch was attempted
      SE.BitVector -- The symbolic branch condition
  deriving (Show, Eq)

-- Create a new branch condition.
newBranch :: SE.BitVector -> Branch
newBranch = Branch False

-- Create a new branch from an existing branch, thereby updating its metadata.
-- It is assumed that the condition, encoded in the branches, is equal.
fromBranch :: Branch -> Branch -> Branch
fromBranch (Branch wasNeg' _) (Branch wasNeg ast) =
  Branch (wasNeg || wasNeg') ast

------------------------------------------------------------------------

-- Represents a single execution through a program, tracking for each
-- symbolic branch condition if it was 'True' or 'False'.
type ExecTrace = [(Bool, Branch)]

-- Create a new empty execution tree.
newExecTrace :: ExecTrace
newExecTrace = []

-- Return all branch conditions of an 'ExecTrace'.
toSExprs :: ExecTrace -> [SMT.SExpr]
toSExprs = map (\(_, Branch _ bv) -> SE.sexpr bv)

-- Append a branch to the execution trace, denoting via a 'Bool'
-- if the branch was taken or if it was not taken.
appendBranch :: ExecTrace -> Bool -> Branch -> ExecTrace
appendBranch trace wasTrue branch = trace ++ [(wasTrue, branch)]

-- Append a constraint to the execution tree. This constraint must
-- be true and, contrary to appendBranch, negation will not be
-- attempted for it.
appendCons :: ExecTrace -> SE.BitVector -> ExecTrace
appendCons trace cons = trace ++ [(True, Branch True cons)]
