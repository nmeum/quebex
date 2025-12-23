-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Exec (execBench) where

import Control.Monad (void)
import Criterion.Main (Benchmark, Benchmarkable, bench, bgroup, nfIO)
import Data.Word (Word64, Word8)
import Language.QBE (parseAndFind)
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.State (SimState, mkEnv, run)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Types qualified as QBE

exec :: [CE.Concolic DE.RegVal] -> String -> IO ()
exec params input = do
  (prog, func) <- parseAndFind entryFunc input

  env <- mkEnv prog 0 1024
  void $ run env (execFunc func params :: SimState (CE.Concolic DE.RegVal) (CE.Concolic Word8) (Maybe (CE.Concolic DE.RegVal)))
  where
    entryFunc :: QBE.GlobalIdent
    entryFunc = QBE.GlobalIdent "entry"

------------------------------------------------------------------------

bubbleSort :: Word64 -> Benchmarkable
bubbleSort inputSize =
  nfIO (readFile "bench/data/Exec/bubble-sort.qbe" >>= exec [E.fromLit (QBE.Base QBE.Word) inputSize])

execBench :: Benchmark
execBench =
  bgroup
    "Concrete Execution"
    [ bench "10" $ bubbleSort 25,
      bench "50" $ bubbleSort 50,
      bench "100" $ bubbleSort 100
    ]
