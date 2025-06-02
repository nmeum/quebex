module Language.QBE (Definition (..), Language.QBE.parse) where

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
    [ DefData <$> dataDef,
      DefType <$> typeDef,
      DefFunc <$> funcDef
    ]

parse :: SourceName -> String -> Either ParseError [Definition]
parse = Text.ParserCombinators.Parsec.parse (many parseDef)
