module Juvix.Compiler.Casm.Translation.FromReg.CasmBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Casm.Data.Builtins
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
  LookupBuiltin :: Builtin -> CasmBuilder m Int
  GetBuiltins :: CasmBuilder m (HashMap Builtin Int)
  SetBuiltins :: HashMap Builtin Int -> CasmBuilder m ()

makeSem ''CasmBuilder

data BuilderState = BuilderState
  { _statePC :: Address,
    _stateAP :: Int,
    _stateVarMap :: HashMap VarRef Int,
    _stateBuiltinMap :: HashMap Builtin Int
  }

makeLenses ''BuilderState

mkBuilderState :: Address -> HashMap VarRef Int -> HashMap Builtin Int -> BuilderState
mkBuilderState addr vars blts =
  BuilderState
    { _statePC = addr,
      _stateAP = 0,
      _stateVarMap = vars,
      _stateBuiltinMap = blts
    }

runCasmBuilder :: Address -> HashMap VarRef Int -> HashMap Builtin Int -> Sem (CasmBuilder ': r) a -> Sem r a
runCasmBuilder addr vars blts = fmap snd . runCasmBuilder' (mkBuilderState addr vars blts)

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
      LookupBuiltin blt -> do
        mp <- gets (^. stateBuiltinMap)
        return $ fromJust $ HashMap.lookup blt mp
      GetBuiltins -> do
        gets (^. stateBuiltinMap)
      SetBuiltins blts -> do
        modify' (set stateBuiltinMap blts)

lookupVar' :: (Member CasmBuilder r) => VarRef -> Sem r Int
lookupVar' = lookupVar >=> return . fromJust

hasVar :: (Member CasmBuilder r) => VarRef -> Sem r Bool
hasVar = lookupVar >=> return . isJust
