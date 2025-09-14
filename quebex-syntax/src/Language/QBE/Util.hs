-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# OPTIONS_GHC -Wno-x-partial #-}

module Language.QBE.Util where

import Data.Word (Word64)
import Language.QBE.Numbers
  ( decimal,
    fractExponent,
    hexnum,
    octnum,
    sign,
  )
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    oneOf,
    skipMany,
    string,
    (<|>),
  )

bind :: String -> a -> Parser a
bind str val = val <$ string str

decNumber :: Parser Word64
decNumber = do
  -- TODO: QBE probably doesn't support a '+' sign prefix
  s <- sign
  s <$> decimal

octNumber :: Parser Word64
octNumber = do
  char '0' >> octnum

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
