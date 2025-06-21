module Language.QBE.Simulator.Symbolic.Tracer
  ( Branch,
    newBranch,
    ExecTrace,
    newExecTrace,
    appendBranch,
    appendCons,
    PathSel,
    newPathSel,
    trackTrace,
    findUnexplored,
  )
where

import Control.Applicative ((<|>))
import Language.QBE.Simulator.Concolic.Expression qualified as C
import Language.QBE.Simulator.Symbolic.Expression qualified as SE
import Language.QBE.Simulator.Tracer qualified as T
import SimpleSMT qualified as SMT

-- Represents a branch condition in the executed code
data Branch
  = MkBranch
      Bool -- Whether negation of the branch was attempted
      SE.BitVector -- The symbolic branch condition
  deriving (Show, Eq)

-- Create a new branch condition.
newBranch :: SE.BitVector -> Branch
newBranch = MkBranch False

-- Create a new branch from an existing branch, thereby updating its metadata.
-- It is assumed that the condition, encoded in the branches, is equal.
fromBranch :: Branch -> Branch -> Branch
fromBranch (MkBranch wasNeg' _) (MkBranch wasNeg ast) = MkBranch (wasNeg || wasNeg') ast

------------------------------------------------------------------------

-- Represents a single execution through a program, tracking for each
-- symbolic branch condition if it was 'True' or 'False'.
type ExecTrace = [(Bool, Branch)]

-- Create a new empty execution tree.
newExecTrace :: ExecTrace
newExecTrace = []

-- Return all branch conditions of an 'ExecTrace'.
toSExprs :: ExecTrace -> [SMT.SExpr]
toSExprs = map (\(_, MkBranch _ bv) -> SE.sexpr bv)

-- Append a branch to the execution trace, denoting via a 'Bool'
-- if the branch was taken or if it was not taken.
appendBranch :: ExecTrace -> Bool -> Branch -> ExecTrace
appendBranch trace wasTrue branch = trace ++ [(wasTrue, branch)]

-- Append a constraint to the execution tree. This constraint must
-- be true and, contrary to appendBranch, negation will not be
-- attempted for it.
appendCons :: ExecTrace -> SE.BitVector -> ExecTrace
appendCons trace cons = trace ++ [(True, MkBranch True cons)]

instance T.Tracer ExecTrace C.Concolic where
  branch t C.Concolic {C.symbolic = Just s} condResult =
    appendBranch t condResult (newBranch s)
  branch t _ _ = t

------------------------------------------------------------------------

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

------------------------------------------------------------------------

-- The 'PathSel' encapsulates data for the Dynamic Symbolic Execution (DSE)
-- algorithm. Specifically for path selection and incremental solving.
data PathSel
  = MkPathSel
      ExecTree -- The current execution tree for the DSE algorithm
      ExecTrace -- The last solved trace, for incremental solving.

-- Create a new empty 'PathSel' object without anything traced yet.
newPathSel :: PathSel
newPathSel = MkPathSel (mkTree []) []

-- Track a new 'ExecTrace' in the 'PathSel'.
trackTrace :: PathSel -> ExecTrace -> PathSel
trackTrace (MkPathSel tree t) trace =
  MkPathSel (addTrace tree trace) t

-- Assignments returned by the Solver for a given query.
type Model = [(SMT.SExpr, SMT.Value)]

-- For a given execution trace, return an assignment (represented
-- as a 'Model') which statisfies all symbolic branch conditions.
-- If such an assignment does not exist, then 'Nothing' is returned.
solveTrace :: SMT.Solver -> PathSel -> ExecTrace -> IO (Maybe Model)
solveTrace solver (MkPathSel _ oldTrace) newTrace = do
  -- Determine the common prefix of the current trace and the old trace
  -- drop constraints beyond this common prefix from the current solver
  -- context. Thereby, keeping the common prefix and making use of
  -- incremental solving capabilities.
  let prefix = prefixLength newTrace oldTrace
  let toDrop = length oldTrace - prefix
  SMT.popMany solver $ fromIntegral toDrop

  -- Only enforce new constraints, i.e. those beyond the common prefix.
  assertTrace (drop prefix newTrace)

  isSat <- SMT.check solver
  case isSat of
    SMT.Sat -> Just <$> SMT.getExprs solver (toSExprs newTrace)
    SMT.Unsat -> pure Nothing
    SMT.Unknown -> error "To-Do: Unknown Result" -- TODO
  where
    -- Add all conditions enforced by the given 'ExecTrace' to the solver.
    -- Returns a list of all asserted conditions.
    assertTrace :: ExecTrace -> IO ()
    assertTrace [] = pure ()
    assertTrace t = do
      let conds = map (\(b, MkBranch _ c) -> SE.toCond b c) t
      mapM_ (\c -> SMT.push solver >> SMT.assert solver c) conds

    -- Determine the length of the common prefix of two lists.
    --
    -- TODO: Move this elsewhere.
    prefixLength :: (Eq a) => [a] -> [a] -> Int
    prefixLength = prefixLength' 0
      where
        prefixLength' :: (Eq a) => Int -> [a] -> [a] -> Int
        prefixLength' n [] _ = n
        prefixLength' n _ [] = n
        prefixLength' n (x : xs) (y : ys)
          | x == y = prefixLength' (n + 1) xs ys
          | otherwise = n

-- Find an assignment that causes exploration of a new execution path through
-- the tested software. This function updates the metadata in the execution
-- tree and thus returns a new execution tree, even if no satisfiable
-- assignment was found.
findUnexplored :: SMT.Solver -> PathSel -> IO (Maybe Model, PathSel)
findUnexplored solver tracer@(MkPathSel tree _) = do
  case negateBranch tree of
    Nothing -> pure (Nothing, tracer)
    Just nt -> do
      let nextTracer = MkPathSel (addTrace tree nt) nt
      res <- solveTrace solver tracer nt
      case res of
        Nothing -> findUnexplored solver nextTracer
        Just m -> pure (Just m, nextTracer)
  where
    -- Negate an unnegated branch in the execution tree and return an
    -- 'ExecTrace' which leads to an unexplored execution path. If no
    -- such path exists, then 'Nothing' is returned. If such a path
    -- exists a concrete variable assignment for it can be calculated
    -- using 'solveTrace'.
    --
    -- The branch node metadata in the resulting 'ExecTree' is updated
    -- to reflect that negation of the selected branch node was attempted.
    -- If further branches are to be negated, the resulting trace should
    -- be added to the 'ExecTree' using 'addTrace' to update the metadata
    -- in the tree as well.
    negateBranch :: ExecTree -> Maybe ExecTrace
    negateBranch Leaf = Nothing
    negateBranch (Node (MkBranch wasNeg ast) Nothing _)
      | wasNeg = Nothing
      | otherwise = Just [(True, MkBranch True ast)]
    negateBranch (Node (MkBranch wasNeg ast) _ Nothing)
      | wasNeg = Nothing
      | otherwise = Just [(False, MkBranch True ast)]
    negateBranch (Node br (Just ifTrue) (Just ifFalse)) =
      do
        -- TODO: Randomly prefer either the left or right child
        (++) [(True, br)] <$> negateBranch ifTrue
        <|> (++) [(False, br)] <$> negateBranch ifFalse
