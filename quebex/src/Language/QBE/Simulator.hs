module Language.QBE.Simulator where

import Control.Monad.Except (ExceptT, liftEither, throwError)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT)
import Data.Map qualified as Map
import Data.Word
import Language.QBE.Generator (generateOperators)
import Language.QBE.Types qualified as QBE

data EvalError
  = TypingError
  | NotImplemented

data RegVal
  = EByte Word8
  | EHalf Word16
  | EWord Word32
  | ELong Word64
  | ESingle Float
  | EDouble Double

addVals :: RegVal -> RegVal -> Either EvalError RegVal
addVals (EByte lhs) (EByte rhs) = Right (EByte $ lhs + rhs)
addVals (EHalf lhs) (EHalf rhs) = Right (EHalf $ lhs + rhs)
addVals (EWord lhs) (EWord rhs) = Right (EWord $ lhs + rhs)
addVals (ELong lhs) (ELong rhs) = Right (ELong $ lhs + rhs)
addVals (ESingle lhs) (ESingle rhs) = Right (ESingle $ lhs + rhs)
addVals (EDouble lhs) (EDouble rhs) = Right (EDouble $ lhs + rhs)
addVals _ _ = Left TypingError

assertType :: QBE.ExtType -> RegVal -> Either EvalError RegVal
assertType QBE.Byte v@(EByte _) = Right v
assertType _ _ = Left TypingError

------------------------------------------------------------------------

type Env = Map.Map String RegVal

lookupValue :: QBE.Value -> Exec RegVal
lookupValue _ = throwError NotImplemented

type Exec a = StateT Env (ExceptT EvalError IO) a

-- execVolatile :: VolatileInstr -> Exec ()
-- execVolatile = _

execInstr :: QBE.ExtType -> QBE.Instr -> Exec RegVal
execInstr retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue lhs
  v2 <- lookupValue rhs
  liftEither (addVals v1 v2) >>= liftEither . assertType retTy
execInstr _ _ = error "not implemented"

-- execStmt :: Statement -> Exec ()
-- execStmt (VolatileInstr v) = execVolatile v
-- execStmt (Assign name ty inst) = execAssign name ty inst

-- execBlock :: Block -> Exec Val
-- execBlock b = _

-- execFunc :: FuncDef -> Exec Val
-- execFunc f = _
