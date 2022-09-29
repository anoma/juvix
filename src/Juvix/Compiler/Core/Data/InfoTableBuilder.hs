module Juvix.Compiler.Core.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Language

data InfoTableBuilder m a where
  FreshSymbol :: InfoTableBuilder m Symbol
  FreshTag :: InfoTableBuilder m Tag
  RegisterIdent :: IdentifierInfo -> InfoTableBuilder m ()
  RegisterConstructor :: ConstructorInfo -> InfoTableBuilder m ()
  RegisterInductive :: InductiveInfo -> InfoTableBuilder m ()
  RegisterIdentNode :: Symbol -> Node -> InfoTableBuilder m ()
  RegisterMain :: Symbol -> InfoTableBuilder m ()
  SetIdentArgsInfo :: Symbol -> [ArgumentInfo] -> InfoTableBuilder m ()
  GetIdent :: Text -> InfoTableBuilder m (Maybe IdentKind)
  GetInfoTable :: InfoTableBuilder m InfoTable

makeSem ''InfoTableBuilder

getConstructorInfo :: Member InfoTableBuilder r => Tag -> Sem r ConstructorInfo
getConstructorInfo tag = do
  tab <- getInfoTable
  return $ fromJust (HashMap.lookup tag (tab ^. infoConstructors))

checkSymbolDefined :: Member InfoTableBuilder r => Symbol -> Sem r Bool
checkSymbolDefined sym = do
  tab <- getInfoTable
  return $ HashMap.member sym (tab ^. identContext)

runInfoTableBuilder :: InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder tab =
  runState tab
    . reinterpret interp
  where
    interp :: InfoTableBuilder m a -> Sem (State InfoTable : r) a
    interp = \case
      FreshSymbol -> do
        s <- get
        modify' (over infoNextSymbol (+ 1))
        return (s ^. infoNextSymbol)
      FreshTag -> do
        s <- get
        modify' (over infoNextTag (+ 1))
        return (UserTag (s ^. infoNextTag))
      RegisterIdent ii -> do
        modify' (over infoIdentifiers (HashMap.insert (ii ^. identifierSymbol) ii))
        whenJust (ii ^? identifierName . _Just . nameText) $ \name ->
          modify' (over identMap (HashMap.insert name (IdentSym (ii ^. identifierSymbol))))
      RegisterInductive i -> do
        modify' (over infoInductives (HashMap.insert (i ^. inductiveSymbol) i))
      RegisterConstructor ci -> do
        modify' (over infoConstructors (HashMap.insert (ci ^. constructorTag) ci))
        modify' (over identMap (HashMap.insert (ci ^. (constructorName . nameText)) (IdentTag (ci ^. constructorTag))))
      RegisterIdentNode sym node ->
        modify' (over identContext (HashMap.insert sym node))
      RegisterMain sym -> do
        modify' (set infoMain (Just sym))
      SetIdentArgsInfo sym argsInfo -> do
        modify' (set (infoIdentifiers . at sym . _Just . identifierArgsInfo) argsInfo)
        modify' (set (infoIdentifiers . at sym . _Just . identifierArgsNum) (length argsInfo))
      GetIdent txt -> do
        s <- get
        return $ HashMap.lookup txt (s ^. identMap)
      GetInfoTable ->
        get
