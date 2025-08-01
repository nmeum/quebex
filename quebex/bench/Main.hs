-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Criterion.Main
import Data.Word (Word32)
import Exec (exec)
import Language.QBE.Simulator.Default.Expression qualified as D

bubbleSort :: Word32 -> Benchmarkable
bubbleSort inputSize =
  whnfIO (readFile "bench/data/bubble-sort.qbe" >>= exec [D.VWord inputSize])

-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup
        "bubble-sort"
        [ bench "10" $ bubbleSort 25,
          bench "50" $ bubbleSort 50,
          bench "100" $ bubbleSort 100
        ]
    ]
