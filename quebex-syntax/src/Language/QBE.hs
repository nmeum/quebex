-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

-- | This module provides top-level definitions for representing programs
-- written in the [QBE](https://c9x.me/compile/) intermediate representation.
module Language.QBE
  ( Program,
    Definition (..),
    globalFuncs,
    Language.QBE.parse,
    ExecError (..),
    parseAndFind,
  )
where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Language.QBE.Parser (dataDef, fileDef, funcDef, skipInitComments, typeDef)
import Language.QBE.Types (DataDef, FuncDef, GlobalIdent, TypeDef, fName)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    SourceName,
    choice,
    eof,
    many,
    parse,
    try,
  )

-- | A QBE program consists of a sequence of definitions. Four types of objects
-- can be defined: aggregate types, data, functions, and debugging information.
--
-- See also: The corresponding section of the [QBE specification](https://c9x.me/compile/doc/il-v1.2.html#Definitions).
data Definition
  = -- | Definition of data (e.g. a string).
    DefData DataDef
  | -- | Definition of an aggregate data type.
    DefType TypeDef
  | -- | Definition of a function.
    DefFunc FuncDef
  | -- | Definition of a debug file.
    DefFile String
  deriving (Eq, Show)

parseDef :: Parser Definition
parseDef =
  choice
    [ DefType <$> typeDef,
      -- Need to try funcDef as both funcDef and
      -- dataDef start with a linkage definition.
      --
      -- TODO: Try parsing linkage then funcDef <|> dataDef.
      DefData <$> try dataDef,
      DefFunc <$> funcDef,
      DefFile <$> fileDef
    ]

-- | A parsed QBE program, represented as a list of 'Definition' values.
type Program = [Definition]

-- | Wrapper to parse a QBE program using 'Text.ParserCombinators.Parsec.parse'.
parse :: SourceName -> String -> Either ParseError Program
parse =
  Text.ParserCombinators.Parsec.parse
    (skipInitComments *> many parseDef <* eof)

-- | Utility function to obtain all functions defined in a QBE 'Program'.
globalFuncs :: Program -> [FuncDef]
globalFuncs = mapMaybe globalFuncs'
  where
    globalFuncs' :: Definition -> Maybe FuncDef
    globalFuncs' (DefFunc f) = Just f
    globalFuncs' _ = Nothing

------------------------------------------------------------------------

-- | Custom 'Exception' used for error handling in 'parseAndFind'.
data ExecError
  = -- | The input is not a valid QBE program.
    ESyntaxError ParseError
  | -- | The given entry function is not defined in the QBE program.
    EUnknownEntry GlobalIdent
  deriving (Show)

instance Exception ExecError

-- | Utility function for the common task of parsing an input as a QBE
-- 'Program' and, within that program, finding the entry function. If the
-- function doesn't exist or a the input is invalid a 'ExecError' exception
-- is thrown.
parseAndFind ::
  (MonadThrow m) =>
  GlobalIdent ->
  String ->
  m (Program, FuncDef)
parseAndFind entryIdent input = do
  prog <- case Language.QBE.parse "" input of -- TODO: file name
    Right rt -> pure rt
    Left err -> throwM $ ESyntaxError err

  let funcs = globalFuncs prog
  func <- case find (\f -> fName f == entryIdent) funcs of
    Just x -> pure x
    Nothing -> throwM $ EUnknownEntry entryIdent

  pure (prog, func)
