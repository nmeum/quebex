module Language.QBE.Simulator.Error where

-- TODO: Arguments and custom Show instances.
data EvalError
  = TypingError
  | UnknownVariable
  | EmptyStack
  | EncounteredHalt
  | InvalidReturnValue
  | UnknownBlock
  | InvalidMemoryLoad
  | UnknownFunction
  | MissingFunctionReturn
  | FunctionReturnIgnored
  | AssignedVoidReturnValue
  deriving (Show, Eq)
