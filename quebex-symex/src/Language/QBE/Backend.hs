-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
module Language.QBE.Backend (Model) where

import SimpleSMT qualified as SMT

-- Assignments returned by the Solver for a given query.
--
-- TODO: Use custom newtype to ensure that we only store Atoms in the 'Model'.
type Model = [(SMT.SExpr, SMT.Value)]
