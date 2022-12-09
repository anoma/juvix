module Juvix.Compiler.Core.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Language

data InfoTableBuilder m a where
  FreshSymbol :: InfoTableBuilder m Symbol
  FreshTag :: InfoTableBuilder m Tag
  RegisterIdent :: Text -> IdentifierInfo -> InfoTableBuilder m ()
  RegisterConstructor :: Text -> ConstructorInfo -> InfoTableBuilder m ()
  RegisterInductive :: Text -> InductiveInfo -> InfoTableBuilder m ()
  RegisterIdentNode :: Symbol -> Node -> InfoTableBuilder m ()
  RegisterMain :: Symbol -> InfoTableBuilder m ()
  OverIdentArgsInfo :: Symbol -> ([ArgumentInfo] -> [ArgumentInfo]) -> InfoTableBuilder m ()
  GetIdent :: Text -> InfoTableBuilder m (Maybe IdentKind)
  GetInfoTable :: InfoTableBuilder m InfoTable

makeSem ''InfoTableBuilder

getConstructorInfo :: Member InfoTableBuilder r => Tag -> Sem r ConstructorInfo
getConstructorInfo tag = do
  tab <- getInfoTable
  return $ fromJust (HashMap.lookup tag (tab ^. infoConstructors))

getInductiveInfo :: Member InfoTableBuilder r => Symbol -> Sem r InductiveInfo
getInductiveInfo sym = do
  tab <- getInfoTable
  return $ fromJust (HashMap.lookup sym (tab ^. infoInductives))

getBoolSymbol :: Member InfoTableBuilder r => Sem r Symbol
getBoolSymbol = do
  ci <- getConstructorInfo (BuiltinTag TagTrue)
  return $ ci ^. constructorInductive

checkSymbolDefined :: Member InfoTableBuilder r => Symbol -> Sem r Bool
checkSymbolDefined sym = do
  tab <- getInfoTable
  return $ HashMap.member sym (tab ^. identContext)

setIdentArgsInfo :: Member InfoTableBuilder r => Symbol -> [ArgumentInfo] -> Sem r ()
setIdentArgsInfo sym = overIdentArgsInfo sym . const

runInfoTableBuilder :: forall r a. InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder tab =
  runState tab
    . reinterpret interp
  where
    interp :: InfoTableBuilder m b -> Sem (State InfoTable : r) b
    interp = \case
      FreshSymbol -> do
        s <- get
        modify' (over infoNextSymbol (+ 1))
        return (s ^. infoNextSymbol)
      FreshTag -> do
        s <- get
        modify' (over infoNextTag (+ 1))
        return (UserTag (s ^. infoNextTag))
      RegisterIdent idt ii -> do
        modify' (over infoIdentifiers (HashMap.insert (ii ^. identifierSymbol) ii))
        modify' (over identMap (HashMap.insert idt (IdentFun (ii ^. identifierSymbol))))
      RegisterConstructor idt ci -> do
        modify' (over infoConstructors (HashMap.insert (ci ^. constructorTag) ci))
        modify' (over identMap (HashMap.insert idt (IdentConstr (ci ^. constructorTag))))
      RegisterInductive idt ii -> do
        modify' (over infoInductives (HashMap.insert (ii ^. inductiveSymbol) ii))
        modify' (over identMap (HashMap.insert idt (IdentInd (ii ^. inductiveSymbol))))
      RegisterIdentNode sym node ->
        modify' (over identContext (HashMap.insert sym node))
      RegisterMain sym -> do
        modify' (set infoMain (Just sym))
      OverIdentArgsInfo sym f -> do
        argsInfo <- f <$> gets (^. infoIdentifiers . at sym . _Just . identifierArgsInfo)
        modify' (set (infoIdentifiers . at sym . _Just . identifierArgsInfo) argsInfo)
        modify' (set (infoIdentifiers . at sym . _Just . identifierArgsNum) (length argsInfo))
        modify' (over infoIdentifiers (HashMap.adjust (over identifierType (expandType (map (^. argumentType) argsInfo))) sym))
      GetIdent txt -> do
        s <- get
        return $ HashMap.lookup txt (s ^. identMap)
      GetInfoTable ->
        get
