-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# LANGUAGE TypeApplications #-}

module Language.QBE.Simulator.Default.State where

import Control.Exception (assert)
import Control.Monad (foldM)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, gets, modify, runStateT)
import Data.Array.IO (IOArray)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word8)
import Language.QBE (Definition (DefData), Program, globalFuncs)
import Language.QBE.Simulator.Default.Expression qualified as D
import Language.QBE.Simulator.Default.Funcs (lookupSimFunc)
import Language.QBE.Simulator.Error as Err
import Language.QBE.Simulator.Expression qualified as E
import Language.QBE.Simulator.Memory qualified as MEM
import Language.QBE.Simulator.State
import Language.QBE.Types qualified as QBE

data Env v b
  = Env
  { envSyms :: Map.Map QBE.GlobalIdent MEM.Address,
    envFuncs :: Map.Map QBE.GlobalIdent QBE.FuncDef,
    envMem :: MEM.Memory IOArray b,
    envStk :: [StackFrame v],
    envStkPtr :: v,
    envDataPtr :: MEM.Address
  }

mkEnv ::
  (MEM.Storable v b, E.ValueRepr v) =>
  Program ->
  MEM.Address ->
  MEM.Size ->
  IO (Env v b)
mkEnv prog a s = do
  -- Memory Layout: Data memory starts at address zero and grows upward. The
  -- stack starts at the maximum address and grows downward towards address
  -- zero.
  --
  -- TODO: Check for stack overflow.
  mem <- MEM.mkMemory a s
  let env =
        Env
          Map.empty
          (makeFuncs $ globalFuncs prog)
          mem
          []
          (E.fromLit (QBE.Base QBE.Long) $ a + s - 1)
          a

  let dataMem = allocData a (mapMaybe isData prog)
  snd <$> runStateT (initData dataMem) env
  where
    makeFuncs :: [QBE.FuncDef] -> Map.Map QBE.GlobalIdent QBE.FuncDef
    makeFuncs = Map.fromList . map (\f -> (QBE.fName f, f))

    isData :: Definition -> Maybe QBE.DataDef
    isData (DefData def) = Just def
    isData _ = Nothing

------------------------------------------------------------------------

-- TODO: Move this to Loader.hs

storeBytes ::
  (MEM.Storable v b) =>
  MEM.Address ->
  [b] ->
  StateT (Env v b) IO MEM.Address
storeBytes addr bytes = do
  mem <- gets envMem
  liftIO $ MEM.storeBytes mem addr bytes
  pure $ addr + fromIntegral (length bytes)
{-# INLINEABLE storeBytes #-}

storeValue ::
  (MEM.Storable v b) =>
  MEM.Address ->
  v ->
  StateT (Env v b) IO MEM.Address
storeValue addr = storeBytes addr . MEM.toBytes
{-# INLINE storeValue #-}

storeValues ::
  (MEM.Storable v b) =>
  MEM.Address ->
  [v] ->
  StateT (Env v b) IO MEM.Address
storeValues addr = storeBytes addr . concatMap MEM.toBytes
{-# INLINE storeValues #-}

loadItem ::
  forall v b.
  (MEM.Storable v b, E.ValueRepr v) =>
  MEM.Address ->
  QBE.ExtType ->
  QBE.DataItem ->
  StateT (Env v b) IO MEM.Address
loadItem addr QBE.Byte (QBE.DString str) = do
  storeValues addr $ E.fromString str
loadItem addr ty (QBE.DSymOff ident off) = do
  globals <- gets envSyms
  case Map.lookup ident globals of
    Nothing -> throwM (Err.UnknownVariable $ show ident)
    Just symAddr ->
      storeValue addr $ E.fromLit @v ty (symAddr + off)
loadItem addr ty (QBE.DConst (QBE.Global ident)) =
  loadItem addr ty (QBE.DSymOff ident 0)
loadItem addr ty (QBE.DConst (QBE.Number num)) =
  storeValue addr $ E.fromLit @v ty num
loadItem addr (QBE.Base QBE.Single) (QBE.DConst (QBE.SFP num)) = do
  storeValue addr $ E.fromFloat @v num
loadItem addr (QBE.Base QBE.Double) (QBE.DConst (QBE.DFP num)) = do
  storeValue addr $ E.fromDouble @v num
loadItem _ _ item = error $ "unsupported DataItem: " ++ show item
{-# INLINEABLE loadItem #-}

loadObj' ::
  forall v b.
  (MEM.Storable v b, E.ValueRepr v) =>
  MEM.Address ->
  QBE.DataObj ->
  StateT (Env v b) IO MEM.Address
loadObj' addr (QBE.OZeroFill n) = do
  let zeroByte = E.fromLit @v QBE.Byte 0
  storeValues addr $ replicate (fromIntegral n) zeroByte
loadObj' addr (QBE.OItem ty items) = do
  foldM (`loadItem` ty) addr items
{-# INLINEABLE loadObj' #-}

-- This invokes loadObj' with alignment. The implementation here must be
-- semantically equivalent to 'QBE.dataSize'.
loadObj ::
  forall v b.
  (MEM.Storable v b, E.ValueRepr v) =>
  MEM.Address ->
  QBE.DataObj ->
  StateT (Env v b) IO MEM.Address
loadObj addr dataObj =
  loadObj' (addr + (addr `mod` QBE.objAlign dataObj)) dataObj
{-# INLINEABLE loadObj #-}

loadData ::
  (MEM.Storable v b, E.ValueRepr v) =>
  MEM.Address ->
  QBE.DataDef ->
  StateT (Env v b) IO ()
loadData addr dataDef = do
  newAddr <- foldM loadObj addr $ QBE.objs dataDef

  -- The address calculations performed by 'loadObj' must be aligned
  -- with those performed by 'allocData' through 'QBE.dataSize'.
  assert (newAddr == addr + fromIntegral (QBE.dataSize dataDef)) $
    pure ()
{-# INLINEABLE loadData #-}

initData ::
  (MEM.Storable v b, E.ValueRepr v) =>
  DataMem ->
  StateT (Env v b) IO ()
initData dataMem = do
  -- Transform the 'DataMem' to 'envSyms', enables forward references.
  modify (\e -> e {envSyms = toSyms dataMem})
  mapM_ (uncurry loadData) dataMem
  where
    toSyms :: DataMem -> Map.Map QBE.GlobalIdent MEM.Address
    toSyms = Map.fromList . map (\(k, v) -> (QBE.name v, k))
{-# SPECIALIZE initData :: DataMem -> StateT (Env D.RegVal Word8) IO () #-}

------------------------------------------------------------------------

-- This code implements the allocation of memory for 'DataDef's. In this
-- case, allocation means assigning a unique non-overlapping 'MEM.Address'.
-- This is separated from the initialization of the memory, which is
-- performed by 'initData'. Decoupling this enables forwards references.
--
-- For example:
--
--  data $a = { l $b }
--  data $b = { b 0 }

-- Specifies the memory layout of the data memory. That is, for each
-- 'DataDef' defined in QBE, it specifies a start address in memory.
type DataMem = [(MEM.Address, QBE.DataDef)]

allocDataDef ::
  QBE.DataDef ->
  (MEM.Address, DataMem) ->
  (MEM.Address, DataMem)
allocDataDef dataDef (startAddr, memMap) =
  let addr =
        MEM.alignAddr startAddr $
          fromMaybe maxAlign (QBE.align dataDef)
      size = fromIntegral $ QBE.dataSize dataDef
   in (addr + size, (addr, dataDef) : memMap)
  where
    -- The alignment of an aggregate type is the maximum alignment the members.
    maxAlign = maximum $ map QBE.objAlign (QBE.objs dataDef)

allocData :: MEM.Address -> [QBE.DataDef] -> DataMem
allocData startAddr dataDefs =
  snd $ foldr allocDataDef (startAddr, []) dataDefs

------------------------------------------------------------------------

-- | Simulator state, parameterized over a value and byte representation.
type SimState v b = StateT (Env v b) IO

instance (MEM.Storable v b, E.ValueRepr v) => Simulator (SimState v b) v where
  isTrue value = pure (E.toWord64 value /= 0)
  toAddress = pure . E.toWord64

  lookupSymbol ident = gets (Map.lookup ident . envSyms)
  findFunc ident = do
    funcs <- gets envFuncs
    pure $ case Map.lookup ident funcs of
      Just x -> Just $ SFuncDef x
      Nothing -> SSimFunc <$> lookupSimFunc ident

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

run :: (E.ValueRepr v, MEM.Storable v b) => Env v b -> SimState v b a -> IO a
run env state = fst <$> runStateT state env
{-# SPECIALIZE run :: Env D.RegVal Word8 -> SimState D.RegVal Word8 a -> IO a #-}
