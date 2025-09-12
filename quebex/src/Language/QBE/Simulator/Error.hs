-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Error where

import Control.Monad.Catch (Exception)
import Language.QBE.Simulator.Memory (Address)
import Language.QBE.Types qualified as QBE

data EvalError
  = TypingError
  | UnknownVariable String
  | EmptyStack
  | EncounteredHalt
  | InvalidReturnValue
  | UnknownBlock QBE.BlockIdent
  | InvalidMemoryLoad
  | UnknownFunction String
  | MissingFunctionReturn
  | FunctionReturnIgnored
  | AssignedVoidReturnValue
  | InvaldSubWordExtension
  | InvalidAddressType
  | OverlappingBlit Address Address
  | FuncArgsMismatch QBE.GlobalIdent
  | InvalidPhiPosition
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
  show (OverlappingBlit a1 a2) = "Addresses for Blit instruction overlap: " ++ show a1 ++ " and " ++ show a2
  show (FuncArgsMismatch ident) = "FuncArgsMismatch: '" ++ show ident ++ "'"
  show InvalidPhiPosition = "InvalidPhiPosition"

instance Exception EvalError
