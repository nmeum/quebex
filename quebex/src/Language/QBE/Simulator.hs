module Language.QBE.Simulator where

import Data.Functor ((<&>))
import Control.Monad (sequence, forM)
import Control.Monad.Except (ExceptT, liftEither, throwError, runExceptT)
import Control.Monad.State (StateT,put,get, runStateT, liftIO)
import Data.Map qualified as Map
import Data.Word
import Language.QBE.Types qualified as QBE

data EvalError
  = TypingError
  | UnknownVariable
  | NotImplemented
  deriving (Show, Eq)

data RegVal
  = EByte Word8
  | EHalf Word16
  | EWord Word32
  | ELong Word64
  | ESingle Float
  | EDouble Double
  deriving (Show, Eq)

addVals :: RegVal -> RegVal -> Either EvalError RegVal
addVals (EByte lhs) (EByte rhs) = Right (EByte $ lhs + rhs)
addVals (EHalf lhs) (EHalf rhs) = Right (EHalf $ lhs + rhs)
addVals (EWord lhs) (EWord rhs) = Right (EWord $ lhs + rhs)
addVals (ELong lhs) (ELong rhs) = Right (ELong $ lhs + rhs)
addVals (ESingle lhs) (ESingle rhs) = Right (ESingle $ lhs + rhs)
addVals (EDouble lhs) (EDouble rhs) = Right (EDouble $ lhs + rhs)
addVals _ _ = Left TypingError

subVals :: RegVal -> RegVal -> Either EvalError RegVal
subVals (EByte lhs) (EByte rhs) = Right (EByte $ lhs - rhs)
subVals (EHalf lhs) (EHalf rhs) = Right (EHalf $ lhs - rhs)
subVals (EWord lhs) (EWord rhs) = Right (EWord $ lhs - rhs)
subVals (ELong lhs) (ELong rhs) = Right (ELong $ lhs - rhs)
subVals (ESingle lhs) (ESingle rhs) = Right (ESingle $ lhs - rhs)
subVals (EDouble lhs) (EDouble rhs) = Right (EDouble $ lhs - rhs)
subVals _ _ = Left TypingError

assertType :: QBE.BaseType -> RegVal -> Either EvalError RegVal
assertType QBE.Word v@(EWord _) = Right v
assertType QBE.Long v@(ELong _) = Right v
assertType QBE.Single v@(ESingle _) = Right v
assertType QBE.Double v@(EDouble _) = Right v
assertType _ _ = Left TypingError

------------------------------------------------------------------------

type Env = Map.Map String RegVal

lookupValue :: QBE.Value -> Exec RegVal
lookupValue (QBE.VConst (QBE.Const (QBE.Number v))) = pure $ EWord (fromIntegral v)
lookupValue (QBE.VConst (QBE.Const (QBE.SFP v))) = error "foo"
lookupValue (QBE.VConst (QBE.Const (QBE.DFP v))) = error "foo"
lookupValue (QBE.VConst (QBE.Const (QBE.Global k))) = error "foo"
lookupValue (QBE.VConst (QBE.Thread k)) = error "not implemented"
lookupValue (QBE.VLocal k) = do
  env <- get
  case Map.lookup k env of
    Nothing -> throwError UnknownVariable
    Just v  -> pure v

type Exec a = StateT Env (ExceptT EvalError IO) a

execVolatile :: QBE.VolatileInstr -> Exec Env
execVolatile = error "execVolatile not implemented"

execInstr :: QBE.BaseType -> QBE.Instr -> Exec RegVal
execInstr retTy (QBE.Add lhs rhs) = do
  v1 <- lookupValue lhs
  v2 <- lookupValue rhs
  liftEither (addVals v1 v2) >>= liftEither . assertType retTy
execInstr retTy (QBE.Sub lhs rhs) = do
  v1 <- lookupValue lhs
  v2 <- lookupValue rhs
  liftEither (subVals v1 v2) >>= liftEither . assertType retTy
execInstr _retTy (QBE.Alloc _size _align) = do
  -- TODO: This needs a byte-addressable memory implementation
  -- needs to return a pointer to the allocated address in memory.
  error "alloc not yet implemented"

execStmt :: QBE.Statement -> Exec Env
execStmt a@(QBE.Assign name ty inst) = do
  rv <- execInstr ty inst
  newEnv <- get <&> Map.insert name rv
  put newEnv >> pure newEnv
execStmt (QBE.Volatile v) = execVolatile v

execBlock :: QBE.Block -> Exec Env
execBlock block = go $ fmap execStmt (QBE.stmt block)
 where
  go []     = get
  go [x]    = x
  go (x:xs) = x >> go xs

-- execFunc :: QBE.FuncDef -> Exec Env
-- execFunc f = 

runExec :: Exec Env -> IO (Either EvalError Env)
runExec env =
  runExceptT (runStateT env Map.empty) <&> fmap fst
