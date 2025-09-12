-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Default.State where

import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, gets, modify, runStateT)
import Data.Array.IO (IOArray)
import Data.Map qualified as Map
import Data.Word (Word8)
import Language.QBE (Program, globalFuncs)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Error as Err
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.State
import Language.QBE.Types qualified as QBE

data Env v b
  = Env
  { envGlobals :: Map.Map QBE.GlobalIdent v,
    envFuncs :: Map.Map QBE.GlobalIdent QBE.FuncDef,
    envMem :: MEM.Memory IOArray b,
    envStk :: [StackFrame v],
    envStkPtr :: v,
    envDataPtr :: MEM.Address,
    envDataSym :: Map.Map QBE.GlobalIdent MEM.Address
  }

loadObj :: MEM.Memory IOArray Word8 -> MEM.Address -> QBE.DataObj -> IO MEM.Address
loadObj mem startAddr (QBE.OZeroFill n) = do
  MEM.storeBytes mem startAddr (replicate (fromIntegral n) 0)
  pure $ startAddr + n
loadObj mem startAddr (QBE.OItem ty items) = do
  E.fromLit

-- loadData :: (E.ValueRepr v, MEM.Storable v b) => Env v b -> QBE.DataDef -> IO (Env v b)
-- loadData env dataDef = do
--   let mem = envMem env
--   let startAddr = alignAddr (envDataPtr env) (QBE.align dataDef)
--   let newDataPtr = foldM (\acc x -> loadItem mem acc x) startAddr (QBE.objs dataDef)

mkEnv ::
  (MEM.Storable v b, E.ValueRepr v) =>
  Program ->
  MEM.Address ->
  MEM.Size ->
  IO (Env v b)
mkEnv prog a s = do
  mem <- MEM.mkMemory a s

  let funcs = globalFuncs prog
  return $ Env Map.empty (makeFuncs funcs) mem [] (E.fromLit QBE.Long $ s - 1) 0 Map.empty

  where
    makeFuncs :: [QBE.FuncDef] -> Map.Map QBE.GlobalIdent QBE.FuncDef
    makeFuncs = Map.fromList . map (\f -> (QBE.fName f, f))

-- | Simulator state, parameterized over a value and byte representation.
type SimState v b = StateT (Env v b) IO

instance (MEM.Storable v b, E.ValueRepr v) => Simulator (SimState v b) v where
  isTrue value = pure (E.toWord64 value /= 0)
  toAddress = pure . E.toWord64

  lookupGlobal ident = gets (Map.lookup ident . envGlobals)
  findFunc ident = gets (fmap SFuncDef . Map.lookup ident . envFuncs)

  activeFrame = do
    stk <- gets envStk
    case stk of
      (x : _) -> pure x
      [] -> throwM Err.EmptyStack
  pushStackFrame frame =
    modify (\s -> s {envStk = frame : envStk s})
  popStackFrame = do
    stk <- gets envStk
    case stk of
      (x : xs) -> modify (\s -> s {envStk = xs}) >> pure x
      [] -> throwM Err.EmptyStack

  getSP = gets envStkPtr
  setSP sp = modify (\s -> s {envStkPtr = sp})

  writeMemory addr extType val = do
    mem <- gets envMem

    -- Since halfwords and bytes are not first class in the IL, storeh and storeb
    -- take a word as argument. Only the first 16 or 8 bits of this word will be
    -- stored in memory at the address specified in the second argument.
    let bytes = MEM.toBytes val
    liftIO $
      MEM.storeBytes mem addr $
        case extType of
          QBE.Byte -> take 1 bytes
          QBE.HalfWord -> take 2 bytes
          QBE.Base _ -> bytes
  readMemory ty addr = do
    mem <- gets envMem
    bytes <- liftIO $ MEM.loadBytes mem addr (QBE.loadByteSize ty)

    case MEM.fromBytes ty bytes of
      Just x -> pure x
      Nothing -> throwM InvalidMemoryLoad

------------------------------------------------------------------------

run :: (E.ValueRepr v, MEM.Storable v b) => Program -> SimState v b a -> IO a
run prog state = do
  emptyEnv <- liftIO $ mkEnv prog 0x0 memSize
  fst <$> runStateT state emptyEnv
  where
    -- TODO: Don't hardcode the empty state.
    memSize = 1024 * 1024 * 10
{-# SPECIALIZE run :: Program -> SimState D.RegVal Word8 a -> IO a #-}
