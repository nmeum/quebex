-- SPDX-FileCopyrightText: 2024 University of Bremen
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Language.QBE.Backend.Tracer
  ( Branch (Branch),
    newBranch,
    fromBranch,
    ExecTrace,
    newExecTrace,
    toSExprs,
    appendBranch,
    appendCons,
    solveTrace,
  )
where

import Control.Exception (throwIO)
import Language.QBE.Backend (SolverError (UnknownResult), prefixLength)
import Language.QBE.Backend.Model qualified as Model
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import SimpleBV qualified as SMT

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
toSExprs = map (\(_, Branch _ bv) -> SE.toSExpr bv)

-- Append a branch to the execution trace, denoting via a 'Bool'
-- if the branch was taken or if it was not taken.
appendBranch :: ExecTrace -> Bool -> Branch -> ExecTrace
appendBranch trace wasTrue branch = trace ++ [(wasTrue, branch)]

-- Append a constraint to the execution tree. This constraint must
-- be true and, contrary to appendBranch, negation will not be
-- attempted for it.
appendCons :: ExecTrace -> SE.BitVector -> ExecTrace
appendCons trace cons = trace ++ [(True, Branch True cons)]

-- For a given execution trace, return an assignment (represented
-- as a 'Model') which statisfies all symbolic branch conditions.
-- If such an assignment does not exist, then 'Nothing' is returned.
--
-- Throws a 'SolverError' on an unknown solver result (e.g., on timeout).
solveTrace :: SMT.Solver -> [SMT.SExpr] -> ExecTrace -> ExecTrace -> IO (Maybe Model.Model)
solveTrace solver inputVars oldTrace newTrace = do
  -- Determine the common prefix of the current trace and the old trace
  -- drop constraints beyond this common prefix from the current solver
  -- context. Thereby, keeping the common prefix and making use of
  -- incremental solving capabilities.
  let prefix = prefixLength newTrace oldTrace
  let toDrop = length oldTrace - prefix
  -- TODO: Don't pop if toDrop is zero.
  SMT.popMany solver $ fromIntegral toDrop

  -- Only enforce new constraints, i.e. those beyond the common prefix.
  assertTrace (drop prefix newTrace)

  isSat <- SMT.check solver
  case isSat of
    SMT.Sat -> Just <$> Model.getModel solver inputVars
    SMT.Unsat -> pure Nothing
    SMT.Unknown -> throwIO UnknownResult
  where
    -- Add all conditions enforced by the given 'ExecTrace' to the solver.
    -- Returns a list of all asserted conditions.
    assertTrace :: ExecTrace -> IO ()
    assertTrace [] = pure ()
    assertTrace t = do
      let conds = map (\(b, Branch _ c) -> SE.toCond b c) t
      mapM_ (\c -> SMT.push solver >> SMT.assert solver c) conds
