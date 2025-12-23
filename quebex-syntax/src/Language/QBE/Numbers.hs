-- SPDX-FileCopyrightText: 1999-2001 Daan Leijen
-- SPDX-FileCopyrightText: 2007 Paolo Martini
-- SPDX-FileCopyrightText: 2013-2014 Christian Maeder <chr.maeder@web.de>
-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: BSD-2-Clause AND GPL-3.0-only

module Language.QBE.Numbers where

import Control.Monad (ap)
import Data.Char (digitToInt)
import Text.Parsec

-- ** float parts

-- | parse a floating point number given the number before a dot, e or E
fractExponent :: (Floating f, Stream s m Char) => Integer -> ParsecT s u m f
fractExponent i = fractExp i True

-- | parse a floating point number given the number before a dot, e or E
fractExp ::
  (Floating f, Stream s m Char) =>
  Integer ->
  Bool ->
  ParsecT s u m f
fractExp i b = genFractExp i (fraction b) exponentFactor

-- | parse a floating point number given the number before the fraction and
-- exponent
genFractExp ::
  (Floating f, Stream s m Char) =>
  Integer ->
  ParsecT s u m f ->
  ParsecT s u m (f -> f) ->
  ParsecT s u m f
genFractExp i frac expo = case fromInteger i of
  f -> genFractAndExp f frac expo <|> fmap ($ f) expo

-- | parse a floating point number given the number before the fraction and
-- exponent that must follow the fraction
genFractAndExp ::
  (Floating f, Stream s m Char) =>
  f ->
  ParsecT s u m f ->
  ParsecT s u m (f -> f) ->
  ParsecT s u m f
genFractAndExp f frac = ap (fmap (flip id . (f +)) frac) . option id

-- | parse a floating point exponent starting with e or E
exponentFactor :: (Floating f, Stream s m Char) => ParsecT s u m (f -> f)
exponentFactor = oneOf "eE" >> extExponentFactor 10 <?> "exponent"

-- | parse a signed decimal and compute the exponent factor given a base.
-- For hexadecimal exponential notation (IEEE 754) the base is 2 and the
-- leading character a p.
extExponentFactor ::
  (Floating f, Stream s m Char) =>
  Int -> ParsecT s u m (f -> f)
extExponentFactor base =
  fmap (flip (*) . exponentValue base) (ap sign (decimal <?> "exponent"))

-- | compute the factor given by the number following e or E. This
-- implementation uses @**@ rather than @^@ for more efficiency for large
-- integers.
exponentValue :: (Floating f) => Int -> Integer -> f
exponentValue base = (fromIntegral base **) . fromInteger

-- ** fractional parts

-- | parse a dot followed by decimal digits as fractional part
fraction :: (Fractional f, Stream s m Char) => Bool -> ParsecT s u m f
fraction b = baseFraction b 10 digit

-- | parse a dot followed by base dependent digits as fractional part
baseFraction ::
  (Fractional f, Stream s m Char) =>
  Bool ->
  Int ->
  ParsecT s u m Char ->
  ParsecT s u m f
baseFraction requireDigit base baseDigit =
  char '.'
    >> fmap
      (fractionValue base)
      ((if requireDigit then many1 else many) baseDigit <?> "fraction")
    <?> "fraction"

-- | compute the fraction given by a sequence of digits following the dot.
-- Only one division is performed and trailing zeros are ignored.
fractionValue :: (Fractional f) => Int -> String -> f
fractionValue base =
  uncurry (/)
    . foldl
      ( \(s, p) d ->
          (p * fromIntegral (digitToInt d) + s, p * fromIntegral base)
      )
      (0, 1)
    . dropWhile (== '0')
    . reverse

-- * integers and naturals

-- | parse a negative or a positive number (returning 'negate' or 'id').
-- positive numbers are NOT allowed to be prefixed by a plus sign.
signMinus :: (Num a, Stream s m Char) => ParsecT s u m (a -> a)
signMinus = (char '-' >> return negate) <|> return id

-- | parse an optional plus or minus sign, returning 'negate' or 'id'
sign :: (Num a, Stream s m Char) => ParsecT s u m (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

-- | parse plain non-negative decimal numbers given by a non-empty sequence
-- of digits
decimal :: (Integral i, Stream s m Char) => ParsecT s u m i
decimal = number 10 digit

-- ** natural parts

-- | parse a hexadecimal number
hexnum :: (Integral i, Stream s m Char) => ParsecT s u m i
hexnum = number 16 hexDigit

-- | parse an octal number
octnum :: (Integral i, Stream s m Char) => ParsecT s u m i
octnum = number 8 octDigit

-- | parse a non-negative number given a base and a parser for the digits
number ::
  (Integral i, Stream s m t) =>
  Int ->
  ParsecT s u m Char ->
  ParsecT s u m i
number base baseDigit = do
  n <- fmap (numberValue base) (many1 baseDigit)
  seq n (return n)

-- | compute the value from a string of digits using a base
numberValue :: (Integral i) => Int -> String -> i
numberValue base =
  foldl (\x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0
