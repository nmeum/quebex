-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: MIT AND GPL-3.0-only

module Main (main) where

import Criterion.Main (defaultMain)
import Exec (execBench)
import SMT (smtBench)

main :: IO ()
main = defaultMain [execBench, smtBench]
