module Juvix.Compiler.Asm.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Language

data IdentKind
  = IdentFun Symbol
  | IdentFwd Symbol
  | IdentInd Symbol
  | IdentConstr Tag

data InfoTableBuilder m a where
  FreshSymbol :: InfoTableBuilder m Symbol
  FreshTag :: InfoTableBuilder m Tag
  RegisterFunction :: FunctionInfo -> InfoTableBuilder m ()
  RegisterConstr :: ConstructorInfo -> InfoTableBuilder m ()
  RegisterInductive :: InductiveInfo -> InfoTableBuilder m ()
  RegisterForward :: Text -> Symbol -> InfoTableBuilder m ()
  RegisterMain :: Symbol -> InfoTableBuilder m ()
  GetIdent :: Text -> InfoTableBuilder m (Maybe IdentKind)
  GetFunctionInfo :: Symbol -> InfoTableBuilder m FunctionInfo

makeSem ''InfoTableBuilder

data BuilderState = BuilderState
  { _stateNextSymbol :: Word,
    _stateNextUserTag :: Word,
    _stateInfoTable :: InfoTable,
    _stateIdents :: HashMap Text IdentKind
  }

makeLenses ''BuilderState

emptyBuilderState :: BuilderState
emptyBuilderState =
  BuilderState
    { _stateNextSymbol = 0,
      _stateNextUserTag = 0,
      _stateInfoTable = emptyInfoTable,
      _stateIdents = mempty
    }

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder =
  fmap (first (^. stateInfoTable))
    . runState emptyBuilderState
    . reinterpret interp
  where
    interp :: InfoTableBuilder m a -> Sem (State BuilderState ': r) a
    interp = \case
      FreshSymbol -> do
        s <- get
        modify' (over stateNextSymbol (+ 1))
        return (s ^. stateNextSymbol)
      FreshTag -> do
        modify' (over stateNextUserTag (+ 1))
        s <- get
        return (UserTag (s ^. stateNextUserTag - 1))
      RegisterFunction fi -> do
        modify' (over (stateInfoTable . infoFunctions) (HashMap.insert (fi ^. functionSymbol) fi))
        modify' (over stateIdents (HashMap.insert (fi ^. functionName) (IdentFun (fi ^. functionSymbol))))
      RegisterConstr ci -> do
        modify' (over (stateInfoTable . infoConstrs) (HashMap.insert (ci ^. constructorTag) ci))
        modify' (over stateIdents (HashMap.insert (ci ^. constructorName) (IdentConstr (ci ^. constructorTag))))
      RegisterInductive ii -> do
        modify' (over (stateInfoTable . infoInductives) (HashMap.insert (ii ^. inductiveSymbol) ii))
        modify' (over stateIdents (HashMap.insert (ii ^. inductiveName) (IdentInd (ii ^. inductiveSymbol))))
      RegisterForward txt sym ->
        modify' (over stateIdents (HashMap.insert txt (IdentFwd sym)))
      RegisterMain sym ->
        modify' (over stateInfoTable (set infoMainFunction (Just sym)))
      GetIdent txt -> do
        s <- get
        return $ HashMap.lookup txt (s ^. stateIdents)
      GetFunctionInfo sym -> do
        s <- get
        return (lookupFunInfo (s ^. stateInfoTable) sym)
