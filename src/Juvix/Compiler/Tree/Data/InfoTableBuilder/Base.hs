{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Tree.Data.InfoTableBuilder.Base where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Language.Base

data IdentKind
  = IdentFun Symbol
  | IdentFwd Symbol
  | IdentInd Symbol
  | IdentConstr Tag

data InfoTableBuilder' t e m a where
  FreshSymbol' :: InfoTableBuilder' t e m Symbol
  FreshTag' :: InfoTableBuilder' t e m Tag
  RegisterFunction' :: FunctionInfo' t e -> InfoTableBuilder' t e m ()
  RegisterConstr' :: ConstructorInfo -> InfoTableBuilder' t e m ()
  RegisterInductive' :: InductiveInfo -> InfoTableBuilder' t e m ()
  RegisterForward' :: Text -> Symbol -> InfoTableBuilder' t e m ()
  RegisterMain' :: Symbol -> InfoTableBuilder' t e m ()
  GetIdent' :: Text -> InfoTableBuilder' t e m (Maybe IdentKind)
  GetFunctionInfo' :: Symbol -> InfoTableBuilder' t e m (FunctionInfo' t e)

makeSem ''InfoTableBuilder'

data BuilderState' t e = BuilderState
  { _stateNextSymbolId :: Word,
    _stateNextUserTag :: Word,
    _stateInfoTable :: InfoTable' t e,
    _stateIdents :: HashMap Text IdentKind
  }

makeLenses ''BuilderState'

emptyBuilderState :: BuilderState' t e
emptyBuilderState =
  BuilderState
    { _stateNextSymbolId = 0,
      _stateNextUserTag = 0,
      _stateInfoTable = emptyInfoTable,
      _stateIdents = mempty
    }

runInfoTableBuilderWithTab :: InfoTable' t e -> Sem (InfoTableBuilder' t e ': r) b -> Sem r (InfoTable' t e, b)
runInfoTableBuilderWithTab tab = fmap (first (^. stateInfoTable)) . runInfoTableBuilder' st
  where
    st =
      BuilderState
        { _stateNextSymbolId = maximum (0 : map (^. symbolId) (HashMap.keys (tab ^. infoFunctions) ++ HashMap.keys (tab ^. infoInductives))),
          _stateNextUserTag = maximum (0 : mapMaybe getUserTagId (HashMap.keys (tab ^. infoConstrs))),
          _stateInfoTable = tab,
          _stateIdents =
            HashMap.fromList $
              map (\fi -> (fi ^. functionName, IdentFun (fi ^. functionSymbol))) (HashMap.elems (tab ^. infoFunctions))
                ++ map (\ii -> (ii ^. inductiveName, IdentInd (ii ^. inductiveSymbol))) (HashMap.elems (tab ^. infoInductives))
                ++ map (\ci -> (ci ^. constructorName, IdentConstr (ci ^. constructorTag))) (HashMap.elems (tab ^. infoConstrs))
        }

runInfoTableBuilder :: Sem (InfoTableBuilder' t e ': r) b -> Sem r (InfoTable' t e, b)
runInfoTableBuilder = fmap (first (^. stateInfoTable)) . runInfoTableBuilder' emptyBuilderState

runInfoTableBuilder' :: forall t e b r. BuilderState' t e -> Sem (InfoTableBuilder' t e ': r) b -> Sem r (BuilderState' t e, b)
runInfoTableBuilder' bs =
  runState bs
    . reinterpret interp
  where
    interp :: forall m b'. InfoTableBuilder' t e m b' -> Sem (State (BuilderState' t e) ': r) b'
    interp = \case
      FreshSymbol' -> do
        s :: BuilderState' t e <- get
        modify' @(BuilderState' t e) (over stateNextSymbolId (+ 1))
        return (Symbol defaultModuleId (s ^. stateNextSymbolId))
      FreshTag' -> do
        modify' @(BuilderState' t e) (over stateNextUserTag (+ 1))
        s <- get @(BuilderState' t e)
        return (UserTag (TagUser defaultModuleId (s ^. stateNextUserTag - 1)))
      RegisterFunction' fi -> do
        modify' (over (stateInfoTable . infoFunctions) (HashMap.insert (fi ^. functionSymbol) fi))
        modify' @(BuilderState' t e) (over stateIdents (HashMap.insert (fi ^. functionName) (IdentFun (fi ^. functionSymbol))))
      RegisterConstr' ci -> do
        modify' @(BuilderState' t e) (over (stateInfoTable . infoConstrs) (HashMap.insert (ci ^. constructorTag) ci))
        modify' @(BuilderState' t e) (over stateIdents (HashMap.insert (ci ^. constructorName) (IdentConstr (ci ^. constructorTag))))
      RegisterInductive' ii -> do
        modify' @(BuilderState' t e) (over (stateInfoTable . infoInductives) (HashMap.insert (ii ^. inductiveSymbol) ii))
        modify' @(BuilderState' t e) (over stateIdents (HashMap.insert (ii ^. inductiveName) (IdentInd (ii ^. inductiveSymbol))))
      RegisterForward' txt sym ->
        modify' @(BuilderState' t e) (over stateIdents (HashMap.insert txt (IdentFwd sym)))
      RegisterMain' sym ->
        modify' @(BuilderState' t e) (over stateInfoTable (set infoMainFunction (Just sym)))
      GetIdent' txt -> do
        s <- get @(BuilderState' t e)
        return $ HashMap.lookup txt (s ^. stateIdents)
      GetFunctionInfo' sym -> do
        s <- get
        return (lookupFunInfo (s ^. stateInfoTable) sym)
