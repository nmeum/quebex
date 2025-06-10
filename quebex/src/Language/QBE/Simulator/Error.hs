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
  deriving (Show, Eq)
