-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Backend
  ( SolverError (..),
    prefixLength,
  )
where

import Control.Exception (Exception)

data SolverError
  = UnknownResult
  deriving (Show)

instance Exception SolverError

------------------------------------------------------------------------

-- | Determine the length of the common prefix of two lists.
prefixLength :: (Eq a) => [a] -> [a] -> Int
prefixLength = prefixLength' 0
  where
    prefixLength' :: (Eq a) => Int -> [a] -> [a] -> Int
    prefixLength' n [] _ = n
    prefixLength' n _ [] = n
    prefixLength' n (x : xs) (y : ys)
      | x == y = prefixLength' (n + 1) xs ys
      | otherwise = n
