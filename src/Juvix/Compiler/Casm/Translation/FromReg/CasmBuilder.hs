module Juvix.Compiler.Casm.Translation.FromReg.CasmBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Reg.Language.Instrs (VarRef)

data CasmBuilder :: Effect where
  IncPC :: Int -> CasmBuilder m ()
  GetPC :: CasmBuilder m Address
  IncAP :: Int -> CasmBuilder m ()
  GetAP :: CasmBuilder m Int
  SetAP :: Address -> CasmBuilder m ()
  InsertVar :: VarRef -> Int -> CasmBuilder m ()
  LookupVar :: VarRef -> CasmBuilder m (Maybe Int)
  GetVars :: CasmBuilder m (HashMap VarRef Int)
  SetVars :: HashMap VarRef Int -> CasmBuilder m ()
  GetBuiltinOffset :: CasmBuilder m Int
  SetBuiltinOffset :: Int -> CasmBuilder m ()

makeSem ''CasmBuilder

data BuilderState = BuilderState
  { _statePC :: Address,
    _stateAP :: Int,
    _stateVarMap :: HashMap VarRef Int,
    _stateBuiltinOff :: Int
  }

makeLenses ''BuilderState

mkBuilderState :: Address -> HashMap VarRef Int -> Int -> BuilderState
mkBuilderState addr vars bltOff =
  BuilderState
    { _statePC = addr,
      _stateAP = 0,
      _stateVarMap = vars,
      _stateBuiltinOff = bltOff
    }

runCasmBuilder :: Address -> HashMap VarRef Int -> Int -> Sem (CasmBuilder ': r) a -> Sem r a
runCasmBuilder addr vars bltOff = fmap snd . runCasmBuilder' (mkBuilderState addr vars bltOff)

runCasmBuilder' :: BuilderState -> Sem (CasmBuilder ': r) a -> Sem r (BuilderState, a)
runCasmBuilder' bs = reinterpret (runState bs) interp
  where
    interp :: CasmBuilder m a -> Sem (State BuilderState ': r) a
    interp = \case
      IncPC i -> do
        modify' (over statePC (+ i))
      GetPC -> do
        gets (^. statePC)
      IncAP i -> do
        modify' (over stateAP (+ i))
      GetAP -> do
        gets (^. stateAP)
      SetAP addr -> do
        modify' (set stateAP addr)
      InsertVar v i -> do
        modify' (over stateVarMap (HashMap.insert v i))
      LookupVar v -> do
        mp <- gets (^. stateVarMap)
        return $ HashMap.lookup v mp
      GetVars -> do
        gets (^. stateVarMap)
      SetVars vars -> do
        modify' (set stateVarMap vars)
      GetBuiltinOffset -> do
        gets (^. stateBuiltinOff)
      SetBuiltinOffset bltOff -> do
        modify' (set stateBuiltinOff bltOff)

lookupVar' :: (Member CasmBuilder r) => VarRef -> Sem r Int
lookupVar' = lookupVar >=> return . fromJust

hasVar :: (Member CasmBuilder r) => VarRef -> Sem r Bool
hasVar = lookupVar >=> return . isJust
