-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Error (EvalError (..)) where

import Control.Monad.Catch (Exception)
import Language.QBE.Simulator.Memory (Address, showAddr)
import Language.QBE.Types qualified as QBE

-- TODO: Differentiate different typing errors.
data EvalError
  = TypingError
  | UnknownVariable String
  | EmptyStack
  | EncounteredHalt
  | InvalidReturnValue
  | UnknownBlock QBE.BlockIdent
  | InvalidMemoryLoad
  | UnknownFunction QBE.GlobalIdent
  | UnknownFunctionAddr Address
  | MissingFunctionReturn
  | FunctionReturnIgnored
  | AssignedVoidReturnValue
  | InvaldSubWordExtension
  | InvalidAddressType
  | OverlappingBlit Address Address
  | FuncArgsMismatch QBE.GlobalIdent
  | InvalidPhiPosition
  | MemoryError String
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
  show (UnknownFunctionAddr addr) = "UnknownFunctionAddr: '" ++ showAddr addr ++ "'"
  show MissingFunctionReturn = "MissingFunctionReturn"
  show FunctionReturnIgnored = "FunctionReturnIgnored"
  show AssignedVoidReturnValue = "AssignedVoidReturnValue"
  show InvaldSubWordExtension = "InvaldSubWordExtension"
  show InvalidAddressType = "InvalidAddressType"
  show (OverlappingBlit a1 a2) = "Addresses for Blit instruction overlap: " ++ show a1 ++ " and " ++ show a2
  show (FuncArgsMismatch ident) = "FuncArgsMismatch: '" ++ show ident ++ "'"
  show InvalidPhiPosition = "InvalidPhiPosition"
  show (MemoryError msg) = "MemoryError: " ++ show msg

instance Exception EvalError
