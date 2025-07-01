-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.SimulatorNew where

import Control.Monad (foldM)
import Control.Monad.Cont (ContT (ContT))
import Data.Map qualified as Map
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Types qualified as QBE

data ExecResult v = Halted | Return (Maybe v)
  deriving (Show)

data ExecState v = JumpState (Env v)
  deriving (Show)

-- Idea:
--  1. Tracing is implemented as a continuation with >=>
--  2. Custom Env state is implemented by making Env a type class
--  3. Function interception is implementation as a continuation
--  4. Error handling is done via MonadError
--
-- Maybe: We could pass Env via a reader.

data Env val
  = Env
  { envGlobals :: Map.Map QBE.GlobalIdent val,
    envLocals :: Map.Map QBE.LocalIdent val
  }
  deriving (Show, Eq)

mkEnv :: (E.ValueRepr v) => Env v
mkEnv = Env Map.empty Map.empty

-- TODO: callback-like api as provided by glut
-- see https://www.haskellforall.com/2012/12/the-continuation-monad.html

lookupValue :: (E.ValueRepr v) => Env v -> QBE.BaseType -> QBE.Value -> Maybe v
lookupValue _ ty (QBE.VConst (QBE.Const (QBE.Number v))) =
  pure $ E.fromLit ty v
lookupValue _ ty (QBE.VConst (QBE.Const (QBE.SFP v))) =
  E.subType ty (E.fromFloat v)
lookupValue _ ty (QBE.VConst (QBE.Const (QBE.DFP v))) =
  E.subType ty (E.fromDouble v)
lookupValue env ty (QBE.VConst (QBE.Const (QBE.Global k))) =
  Map.lookup k (envGlobals env) >>= E.subType ty
lookupValue _ _ _ =
  error "lookup not implemented"

runBinary :: (E.ValueRepr v) => QBE.BaseType -> (v -> v -> Maybe v) -> v -> v -> Maybe v
runBinary ty op lhs rhs = do
  lhs' <- E.subType ty lhs
  rhs' <- E.subType ty rhs
  op lhs' rhs'

execInstr :: (E.ValueRepr v) => Env v -> QBE.BaseType -> QBE.Instr -> Maybe v
execInstr env retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue env retTy lhs
  v2 <- lookupValue env retTy rhs
  runBinary retTy E.add v1 v2
execInstr _ _ _ = error "execInstr not implemented"

modifyFrame :: (E.ValueRepr v) => Env v -> QBE.LocalIdent -> v -> Env v
modifyFrame env@Env {envLocals = locals} name value =
  env {envLocals = Map.insert name value locals}

-- TODO: change this to execStmt
execStmt :: (E.ValueRepr v) => Env v -> QBE.Statement -> ContT (ExecResult v) IO (Env v)
execStmt env (QBE.Assign name ty inst) =
  ContT $ \cont ->
    do
      case execInstr env ty inst of
        Nothing -> error "invalid instr"
        Just val -> cont (modifyFrame env name val)
execStmt _ _ = error "statement not implemented"

execStmts :: (E.ValueRepr v) => Env v -> [QBE.Statement] -> ContT (ExecResult v) IO (Env v)
execStmts env stmts = foldM (\acc x -> execStmt acc x) env stmts

-- execStmts env [] = ContT $ \cont -> cont env
-- execStmts env (stmt:rest) = execStmt env stmt >>= flip execStmts rest

execJump :: (E.ValueRepr v) => Env v -> QBE.JumpInstr -> ContT (ExecResult v) IO (ExecState v)
execJump _ QBE.Halt = ContT $ \_ -> pure Halted
execJump env _instr = ContT $ \cont -> (cont $ JumpState env)

-- execJump env instr = error "jump not implemented"

execBlock :: (E.ValueRepr v) => Env v -> QBE.Block -> ContT (ExecResult v) IO (ExecState v)
execBlock env block = execStmts env (QBE.stmt block) >>= (\e -> execJump e (QBE.term block))
