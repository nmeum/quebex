-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only
{-# LANGUAGE TypeApplications #-}

module Language.QBE.Simulator.Default.State where

import Control.Monad (foldM)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, gets, modify, runStateT)
import Data.Array.IO (IOArray)
import Data.Char (ord)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word64, Word8)
import Language.QBE (Definition (DefData), Program, globalFuncs)
import Language.QBE.Simulator.Default.Expression qualified as D
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
  -- Memory Layout: Data memory starts add address zero. The
  -- stack starts at the maximum address and grows downward
  -- towards address zero.
  --
  -- TODO: Check for stack overflow.
  mem <- MEM.mkMemory a s
  let env =
        Env
          Map.empty
          (makeFuncs $ globalFuncs prog)
          mem
          []
          (E.fromLit (QBE.Base QBE.Long) $ s - 1)
          0

  let dataDefs = mapMaybe isData prog
  snd <$> runStateT (mapM loadData dataDefs) env
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
loadItem addr ty (QBE.DString str) = do
  let string = map (\c -> E.fromLit @v ty (fromIntegral $ ord c)) str
  storeValues addr string
loadItem addr ty (QBE.DSymOff ident off) = do
  -- TODO: Support recursive DataDef's (e.g., `data $c = { l $c }`).
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

loadObj ::
  forall v b.
  (MEM.Storable v b, E.ValueRepr v) =>
  MEM.Address ->
  QBE.DataObj ->
  StateT (Env v b) IO MEM.Address
loadObj addr (QBE.OZeroFill n) = do
  let zeroByte = E.fromLit @v QBE.Byte 0
  storeValues addr $ replicate (fromIntegral n) zeroByte
loadObj addr (QBE.OItem ty items) = do
  foldM (`loadItem` ty) addr items
{-# INLINEABLE loadObj #-}

loadData ::
  (MEM.Storable v b, E.ValueRepr v) =>
  QBE.DataDef ->
  StateT (Env v b) IO ()
loadData dataDef = do
  startAddr <- gets envDataPtr
  let addr =
        MEM.alignAddr startAddr $
          fromMaybe maxAlign (QBE.align dataDef)

  newAddr <- foldM loadObj addr $ QBE.objs dataDef
  let symName = QBE.name dataDef

  modify
    ( \e ->
        e
          { envSyms = Map.insert symName addr (envSyms e),
            envDataPtr = newAddr
          }
    )
  where
    -- TODO
    maxAlign :: Word64
    maxAlign = 4
{-# SPECIALIZE loadData :: QBE.DataDef -> StateT (Env D.RegVal Word8) IO () #-}

------------------------------------------------------------------------

-- | Simulator state, parameterized over a value and byte representation.
type SimState v b = StateT (Env v b) IO

instance (MEM.Storable v b, E.ValueRepr v) => Simulator (SimState v b) v where
  isTrue value = pure (E.toWord64 value /= 0)
  toAddress = pure . E.toWord64

  lookupSymbol ident = gets (Map.lookup ident . envSyms)
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
