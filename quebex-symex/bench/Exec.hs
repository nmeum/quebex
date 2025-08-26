-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

-- TODO: Code duplication with quebex/bench/{Main.hs,Exec.hs}
module Exec (execBench) where

import Control.Monad (void)
import Criterion.Main
import Data.List (find)
import Data.Word (Word64, Word8)
import Language.QBE (globalFuncs, parse)
import Language.QBE.Simulator (execFunc)
import Language.QBE.Simulator.Concolic.Expression qualified as CE
import Language.QBE.Simulator.Default.Expression qualified as DE
import Language.QBE.Simulator.Default.State (SimState, run)
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Types qualified as QBE

entryFunc :: QBE.GlobalIdent
entryFunc = QBE.GlobalIdent "entry"

exec :: [CE.Concolic DE.RegVal] -> String -> IO ()
exec params input = do
  prog <- case parse "input" input of
    Left err -> fail $ "parsing error: " ++ show err
    Right pr -> pure pr

  let funcs = globalFuncs prog
  func <- case find (\f -> QBE.fName f == entryFunc) funcs of
    Just x -> pure x
    Nothing -> fail $ "unknown function: " ++ show entryFunc

  void $ run prog (execFunc func params :: SimState (CE.Concolic DE.RegVal) (CE.Concolic Word8) (Maybe (CE.Concolic DE.RegVal)))

------------------------------------------------------------------------

bubbleSort :: Word64 -> Benchmarkable
bubbleSort inputSize =
  nfIO (readFile "bench/data/Exec/bubble-sort.qbe" >>= exec [E.fromLit QBE.Word inputSize])

execBench :: Benchmark
execBench =
  bgroup
    "Concrete Execution"
    [ bench "10" $ bubbleSort 25,
      bench "50" $ bubbleSort 50,
      bench "100" $ bubbleSort 100
    ]
