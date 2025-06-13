module Language.QBE
  ( Program,
    Definition (..),
    globalFuncs,
    Language.QBE.parse,
  )
where

import Data.Maybe (catMaybes)
import Language.QBE.Parser (dataDef, funcDef, typeDef)
import Language.QBE.Types (DataDef, FuncDef, TypeDef)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    SourceName,
    choice,
    many,
    parse,
    eof
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
globalFuncs = catMaybes . map globalFuncs'
  where
    globalFuncs' :: Definition -> Maybe FuncDef
    globalFuncs' (DefFunc f) = Just f
    globalFuncs' _ = Nothing

parse :: SourceName -> String -> Either ParseError Program
parse = Text.ParserCombinators.Parsec.parse (many parseDef <* eof)
