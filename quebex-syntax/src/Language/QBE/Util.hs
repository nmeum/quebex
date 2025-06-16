{-# OPTIONS_GHC -Wno-x-partial #-}

module Language.QBE.Util where

import Data.Word (Word64)
import Numeric (readDec)
import Language.QBE.Numbers (decimal, fractExponent, hexnum, sign)
import Text.ParserCombinators.Parsec
  ( Parser,
    digit,
    many1,
    oneOf,
    skipMany,
    string,
    (<|>),
  )

readWord64 :: String -> Word64
readWord64 = fst . head . readDec

------------------------------------------------------------------------

bind :: String -> a -> Parser a
bind str val = val <$ string str

decNumber :: Parser Word64
decNumber = do
  -- TODO: QBE probably doesn't support a '+' sign prefix
  s <- sign
  n <- many1 digit
  return (s $ readWord64 n)

-- A float parser that tries to be compatible with strtod(3).
float :: (Floating f, Read f) => Parser f
float = do
  _ <- skipSpace
  s <- sign
  -- TODO: Support infininty and NaN
  (decimal <|> hexnum) >>= fractExponent . s
  where
    -- See musl's isspace(3) implementation.
    skipSpace :: Parser ()
    skipSpace = skipMany $ oneOf " \t\n\v\f\r"
