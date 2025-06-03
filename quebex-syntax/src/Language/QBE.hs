module Language.QBE (Program, Definition (..), Language.QBE.parse) where

import Language.QBE.Parser (dataDef, funcDef, typeDef)
import Language.QBE.Types (DataDef, FuncDef, TypeDef)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    SourceName,
    choice,
    parse, many,
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

parse :: SourceName -> String -> Either ParseError Program
parse = Text.ParserCombinators.Parsec.parse (many parseDef)
