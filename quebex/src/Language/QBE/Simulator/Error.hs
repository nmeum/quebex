module Language.QBE.Simulator.Error where

-- TODO: Arguments and custom Show instances.
data EvalError
  = TypingError
  | UnknownVariable
  | EmptyStack
  | NotImplemented
  deriving (Show, Eq)
