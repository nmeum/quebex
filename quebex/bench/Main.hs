-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Main (main) where

import Control.Monad (void)
import Criterion.Main (Benchmarkable, bench, bgroup, defaultMain, nfIO)
import Data.Word (Word32, Word64, Word8)
import Language.QBE (parseAndFind)
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Default.State (Env, mkEnv, run)
import Language.QBE.Types qualified as QBE

exec :: [D.RegVal] -> String -> IO ()
exec params input = do
  (prog, func) <- parseAndFind entryFunc input

  env <- mkEnv prog 0 memSize :: IO (Env D.RegVal Word8)
  void $ run env (execFunc func params)
  where
    memSize :: Word64
    memSize = 1024 * 1024 * 10

    entryFunc :: QBE.GlobalIdent
    entryFunc = QBE.GlobalIdent "entry"

bubbleSort :: Word32 -> Benchmarkable
bubbleSort inputSize =
  nfIO (readFile "bench/data/bubble-sort.qbe" >>= exec [D.VWord inputSize])

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
