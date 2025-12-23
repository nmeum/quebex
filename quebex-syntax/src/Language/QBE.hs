-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

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
import Language.QBE.Parser (dataDef, funcDef, skipInitComments, typeDef)
import Language.QBE.Types (DataDef, FuncDef, GlobalIdent, TypeDef, fName)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    SourceName,
    choice,
    eof,
    many,
    parse,
  )

data Definition
  = DefData DataDef
  | DefType TypeDef
  | DefFunc FuncDef
  deriving (Eq, Show)

parseDef :: Parser Definition
parseDef =
  choice
    [ DefType <$> typeDef,
      DefFunc <$> funcDef,
      DefData <$> dataDef
    ]

type Program = [Definition]

globalFuncs :: Program -> [FuncDef]
globalFuncs = mapMaybe globalFuncs'
  where
    globalFuncs' :: Definition -> Maybe FuncDef
    globalFuncs' (DefFunc f) = Just f
    globalFuncs' _ = Nothing

parse :: SourceName -> String -> Either ParseError Program
parse =
  Text.ParserCombinators.Parsec.parse
    (skipInitComments *> many parseDef <* eof)

------------------------------------------------------------------------

data ExecError
  = ESyntaxError ParseError
  | EUnknownEntry GlobalIdent
  deriving (Show)

instance Exception ExecError

-- | Utility function for the common task of parsing an input as a QBE
-- 'Program' and, within that program, finding the entry function. If the
-- function doesn't exist or a the input is invalid an exception is thrown.
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
