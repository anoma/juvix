module Juvix.Compiler.Concrete.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude

data InfoTableBuilder :: Effect where
  RegisterAxiom :: AxiomDef 'Scoped -> InfoTableBuilder m ()
  RegisterConstructor :: ConstructorDef 'Scoped -> InfoTableBuilder m ()
  RegisterInductive :: InductiveDef 'Scoped -> InfoTableBuilder m ()
  RegisterFunctionDef :: FunctionDef 'Scoped -> InfoTableBuilder m ()
  RegisterName :: (HasLoc c) => S.Name' c -> InfoTableBuilder m ()
  RegisterScopedIden :: ScopedIden -> InfoTableBuilder m ()
  RegisterModuleDoc :: S.NameId -> Maybe (Judoc 'Scoped) -> InfoTableBuilder m ()
  RegisterFixity :: FixityDef -> InfoTableBuilder m ()
  RegisterPrecedence :: S.NameId -> S.NameId -> InfoTableBuilder m ()
  RegisterHighlightDoc :: S.NameId -> Maybe (Judoc 'Scoped) -> InfoTableBuilder m ()
  RegisterNameSig :: S.NameId -> NameSignature 'Scoped -> InfoTableBuilder m ()
  RegisterConstructorSig :: S.NameId -> RecordNameSignature 'Scoped -> InfoTableBuilder m ()
  RegisterParsedNameSig :: S.NameId -> NameSignature 'Parsed -> InfoTableBuilder m ()
  RegisterParsedConstructorSig :: S.NameId -> RecordNameSignature 'Parsed -> InfoTableBuilder m ()
  RegisterRecordInfo :: S.NameId -> RecordInfo -> InfoTableBuilder m ()
  RegisterAlias :: S.NameId -> PreSymbolEntry -> InfoTableBuilder m ()
  GetInfoTable :: InfoTableBuilder m InfoTable

makeSem ''InfoTableBuilder

registerDoc :: forall r. (Members '[HighlightBuilder, State InfoTable] r) => NameId -> Maybe (Judoc 'Scoped) -> Sem r ()
registerDoc k md = do
  modify (set (highlightDoc . at k) md)

runInfoTableBuilder :: (Member HighlightBuilder r) => InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder ini = reinterpret (runState ini) $ \case
  RegisterAxiom d ->
    let j = d ^. axiomDoc
     in do
          modify' (over infoAxioms (HashMap.insert (d ^. axiomName . nameId) d))
          registerDoc (d ^. axiomName . nameId) j
  RegisterConstructor c ->
    let j = c ^. constructorDoc
     in do
          modify' (over infoConstructors (HashMap.insert (c ^. constructorName . nameId) c))
          registerDoc (c ^. constructorName . nameId) j
  RegisterInductive ity ->
    let j = ity ^. inductiveDoc
     in do
          modify' (over infoInductives (HashMap.insert (ity ^. inductiveName . nameId) ity))
          registerDoc (ity ^. inductiveName . nameId) j
  RegisterFunctionDef f ->
    let j = f ^. signDoc
     in do
          modify' (over infoFunctions (HashMap.insert (f ^. signName . nameId) f))
          registerDoc (f ^. signName . nameId) j
  RegisterName n -> do
    modify (over highlightNames (cons (S.anameFromName n)))
  RegisterScopedIden n -> do
    modify (over highlightNames (cons (anameFromScopedIden n)))
  RegisterModuleDoc uid doc -> do
    registerDoc uid doc
  RegisterFixity f -> do
    let sid = f ^. fixityDefSymbol . S.nameId
    modify (over infoFixities (HashMap.insert sid f))
    case f ^. fixityDefFixity . fixityId of
      Just fid -> modify (over infoPrecedenceGraph (HashMap.alter (Just . fromMaybe mempty) fid))
      Nothing -> return ()
  RegisterPrecedence l h ->
    modify (over infoPrecedenceGraph (HashMap.alter (Just . HashSet.insert h . fromMaybe mempty) l))
  RegisterHighlightDoc fid doc ->
    registerDoc fid doc
  RegisterNameSig uid sig ->
    modify (over infoNameSigs (HashMap.insert uid sig))
  RegisterConstructorSig uid sig ->
    modify (over infoConstructorSigs (HashMap.insert uid sig))
  RegisterParsedNameSig uid sig ->
    modify (over infoParsedNameSigs (HashMap.insert uid sig))
  RegisterParsedConstructorSig uid sig ->
    modify (over infoParsedConstructorSigs (HashMap.insert uid sig))
  RegisterRecordInfo uid recInfo ->
    modify (over infoRecords (HashMap.insert uid recInfo))
  RegisterAlias uid a ->
    modify (over infoScoperAlias (HashMap.insert uid a))
  GetInfoTable ->
    get

runInfoTableBuilderRepl :: InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilderRepl tab = ignoreHighlightBuilder . runInfoTableBuilder tab . raiseUnder

ignoreInfoTableBuilder :: (Member HighlightBuilder r) => Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = fmap snd . runInfoTableBuilder mempty

anameFromScopedIden :: ScopedIden -> AName
anameFromScopedIden s =
  AName
    { _anameLoc = getLoc s,
      _anameKind = getNameKind s,
      _anameDocId = s ^. scopedIdenFinal . nameId,
      _anameDefinedLoc = s ^. scopedIdenSrcName . nameDefined,
      _anameVerbatim = s ^. scopedIdenSrcName . nameVerbatim
    }

lookupInfo :: (Members '[InfoTableBuilder, Reader InfoTable] r) => (InfoTable -> Maybe a) -> Sem r a
lookupInfo f = do
  tab1 <- ask
  fromMaybe (fromJust (f tab1)) . f <$> getInfoTable

lookupFixity :: (Members '[InfoTableBuilder, Reader InfoTable] r) => S.NameId -> Sem r FixityDef
lookupFixity uid = lookupInfo (HashMap.lookup uid . (^. infoFixities))

getPrecedenceGraph :: (Members '[InfoTableBuilder, Reader InfoTable] r) => Sem r PrecedenceGraph
getPrecedenceGraph = do
  tab <- ask
  tab' <- getInfoTable
  return $ combinePrecedenceGraphs (tab ^. infoPrecedenceGraph) (tab' ^. infoPrecedenceGraph)
