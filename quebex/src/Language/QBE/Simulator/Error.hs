module Language.QBE.Simulator.Error where

import Language.QBE.Types qualified as QBE

data EvalError
  = TypingError
  | UnknownVariable String
  | EmptyStack
  | EncounteredHalt
  | InvalidReturnValue
  | UnknownBlock QBE.BlockIdent
  | InvalidMemoryLoad
  | UnknownFunction QBE.GlobalIdent
  | MissingFunctionReturn
  | FunctionReturnIgnored
  | AssignedVoidReturnValue
  | InvaldSubWordExtension
  | InvalidAddressType
  deriving (Eq)

instance Show EvalError where
  show TypingError = "TypingError"
  show (UnknownVariable s) = "UnknownVariable: '" ++ show s ++ "'"
  show EmptyStack = "EmptyStack"
  show EncounteredHalt = "EncounteredHalt"
  show InvalidReturnValue = "InvalidReturnValue"
  show (UnknownBlock block) = "UnknownBlock: '" ++ show block ++ "'"
  show InvalidMemoryLoad = "InvalidMemoryLoad"
  show (UnknownFunction ident) = "UnknownFunction: '" ++ show ident ++ "'"
  show MissingFunctionReturn = "MissingFunctionReturn"
  show FunctionReturnIgnored = "FunctionReturnIgnored"
  show AssignedVoidReturnValue = "AssignedVoidReturnValue"
  show InvaldSubWordExtension = "InvaldSubWordExtension"
  show InvalidAddressType = "InvalidAddressType"
