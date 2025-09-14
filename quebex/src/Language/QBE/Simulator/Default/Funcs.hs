-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Default.Funcs (lookupSimFunc) where

import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Language.QBE.Simulator.Error (EvalError (FuncArgsMismatch))
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.State (Simulator, readNullArray, toAddress)
import Language.QBE.Types qualified as QBE

puts :: (MonadIO m, E.ValueRepr v, Simulator m v) => QBE.GlobalIdent -> [v] -> m (Maybe v)
puts _ [strPtr] = do
  bytes <- toAddress strPtr >>= readNullArray
  liftIO $ putStrLn (E.toString bytes)
  pure (Just $ E.fromLit (QBE.Base QBE.Word) 0)
puts ident _ = throwM $ FuncArgsMismatch ident

------------------------------------------------------------------------

-- TODO: Register functions dynamically.
lookupSimFunc ::
  (MonadIO m, E.ValueRepr v, Simulator m v) =>
  QBE.GlobalIdent ->
  Maybe ([v] -> m (Maybe v))
lookupSimFunc i@(QBE.GlobalIdent "puts") = Just (puts i)
lookupSimFunc _ = Nothing
