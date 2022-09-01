module Juvix.Compiler.Core.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Language

data InfoTableBuilder m a where
  FreshSymbol :: InfoTableBuilder m Symbol
  FreshTag :: InfoTableBuilder m Tag
  RegisterIdent :: IdentifierInfo -> InfoTableBuilder m ()
  RegisterConstructor :: ConstructorInfo -> InfoTableBuilder m ()
  RegisterIdentNode :: Symbol -> Node -> InfoTableBuilder m ()
  SetIdentArgsInfo :: Symbol -> [ArgumentInfo] -> InfoTableBuilder m ()
  GetIdent :: Text -> InfoTableBuilder m (Maybe (Either Symbol Tag))
  GetInfoTable :: InfoTableBuilder m InfoTable

makeSem ''InfoTableBuilder

hasIdent :: Member InfoTableBuilder r => Text -> Sem r Bool
hasIdent txt = do
  i <- getIdent txt
  case i of
    Just _ -> return True
    Nothing -> return False

getConstructorInfo :: Member InfoTableBuilder r => Tag -> Sem r ConstructorInfo
getConstructorInfo tag = do
  tab <- getInfoTable
  return $ fromJust (HashMap.lookup tag (tab ^. infoConstructors))

checkSymbolDefined :: Member InfoTableBuilder r => Symbol -> Sem r Bool
checkSymbolDefined sym = do
  tab <- getInfoTable
  return $ HashMap.member sym (tab ^. identContext)

data BuilderState = BuilderState
  { _stateNextSymbol :: Word,
    _stateNextUserTag :: Word,
    _stateInfoTable :: InfoTable
  }

makeLenses ''BuilderState

initBuilderState :: InfoTable -> BuilderState
initBuilderState tab =
  BuilderState
    { _stateNextSymbol = fromIntegral $ HashMap.size (tab ^. infoIdentifiers),
      _stateNextUserTag = fromIntegral $ HashMap.size (tab ^. infoConstructors),
      _stateInfoTable = tab
    }

runInfoTableBuilder :: InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder tab =
  fmap (first (^. stateInfoTable))
    . runState (initBuilderState tab)
    . reinterpret interp
  where
    interp :: InfoTableBuilder m a -> Sem (State BuilderState : r) a
    interp = \case
      FreshSymbol -> do
        modify' (over stateNextSymbol (+ 1))
        s <- get
        return (s ^. stateNextSymbol - 1)
      FreshTag -> do
        modify' (over stateNextUserTag (+ 1))
        s <- get
        return (UserTag (s ^. stateNextUserTag - 1))
      RegisterIdent ii -> do
        modify' (over stateInfoTable (over infoIdentifiers (HashMap.insert (ii ^. identifierSymbol) ii)))
        modify' (over stateInfoTable (over identMap (HashMap.insert (ii ^. (identifierName . nameText)) (Left (ii ^. identifierSymbol)))))
      RegisterConstructor ci -> do
        modify' (over stateInfoTable (over infoConstructors (HashMap.insert (ci ^. constructorTag) ci)))
        modify' (over stateInfoTable (over identMap (HashMap.insert (ci ^. (constructorName . nameText)) (Right (ci ^. constructorTag)))))
      RegisterIdentNode sym node ->
        modify' (over stateInfoTable (over identContext (HashMap.insert sym node)))
      SetIdentArgsInfo sym argsInfo -> do
        modify' (over stateInfoTable (over infoIdentifiers (HashMap.adjust (set identifierArgsInfo argsInfo) sym)))
        modify' (over stateInfoTable (over infoIdentifiers (HashMap.adjust (set identifierArgsNum (length argsInfo)) sym)))
      GetIdent txt -> do
        s <- get
        return $ HashMap.lookup txt (s ^. (stateInfoTable . identMap))
      GetInfoTable ->
        get <&> (^. stateInfoTable)
