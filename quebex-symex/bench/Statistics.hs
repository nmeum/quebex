-- SPDX-FileCopyrightText: 2008 Johan Tibell <johan.tibell@google.com>
--
-- SPDX-License-Identifier: BSD-3-Clause

module Statistics
  ( mean,
    stddev,
  )
where

mean :: (Floating a) => [a] -> a
mean = go 0 0
  where
    go :: (Floating a) => a -> Int -> [a] -> a
    go m _ [] = m
    go m n (x : xs) = go (m + (x - m) / fromIntegral (n + 1)) (n + 1) xs

stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ variance xs

variance :: (Floating a) => [a] -> a
variance xs = go 0 0 0 xs / fromIntegral (length xs - 1)
  where
    go :: (Floating a) => a -> Int -> a -> [a] -> a
    go _ _ s [] = s
    go m n s (x : xs') = go nm (n + 1) (s + delta * (x - nm)) xs'
      where
        delta = x - m
        nm = m + delta / fromIntegral (n + 1)
