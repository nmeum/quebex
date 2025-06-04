module Language.QBE.Simulator where

import Data.Word
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
import qualified Data.Map as Map
import qualified Language.QBE.Types as QBE

data RegVal
  = EByte Word8
  | EHalf Word16
  | EWord Word32
  | ELong Word64
  | ESingle Float
  | EDouble Double

addVals :: RegVal -> RegVal -> Maybe RegVal
addVals (EByte lhs) (EByte rhs) = Just (EByte $ lhs + rhs)
addVals (EHalf lhs) (EHalf rhs) = Just (EHalf $ lhs + rhs)
addVals (EWord lhs) (EWord rhs) = Just (EWord $ lhs + rhs)
addVals (ELong lhs) (ELong rhs) = Just (ELong $ lhs + rhs)
addVals (ESingle lhs) (ESingle rhs) = Just (ESingle $ lhs + rhs)
addVals (EDouble lhs) (EDouble rhs) = Just (EDouble $ lhs + rhs)
addVals _ _ = Nothing

assertType :: QBE.ExtType -> RegVal -> Maybe RegVal
assertType QBE.Byte v@(EByte _) = Just v
assertType _ _ = Nothing

------------------------------------------------------------------------

type Env = Map.Map String RegVal

lookupValue :: Env -> QBE.Value -> Maybe RegVal
lookupValue = error "not implemented"

type Exec a = StateT Env (ExceptT RegVal IO)

-- execVolatile :: VolatileInstr -> Exec ()
-- execVolatile = _

-- QBE.ExtType gibt mir den return type der Instruktion an.
execInstr :: Env -> QBE.ExtType -> QBE.Instr -> Maybe RegVal
execInstr env retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue env lhs
  v2 <- lookupValue env rhs
  addVals v1 v2 >>= assertType retTy
execInstr _ _ _ = error "not implemented"

-- execStmt :: Statement -> Exec ()
-- execStmt (VolatileInstr v) = execVolatile v
-- execStmt (Assign name ty inst) = execAssign name ty inst

-- execBlock :: Block -> Exec Val
-- execBlock b = _

-- execFunc :: FuncDef -> Exec Val
-- execFunc f = _
