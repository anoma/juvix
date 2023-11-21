module Juvix.Compiler.Concrete.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data InfoTableBuilder m a where
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
  GetInfoTable :: InfoTableBuilder m InfoTable

makeSem ''InfoTableBuilder

registerDoc :: forall r. (Member (State InfoTable) r) => NameId -> Maybe (Judoc 'Scoped) -> Sem r ()
registerDoc k md = modify (set (infoHighlightDoc . at k) md)

toState :: Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let j = d ^. axiomDoc
     in do
          registerDoc (d ^. axiomName . nameId) j
  RegisterConstructor c ->
    let j = c ^. constructorDoc
     in do
          registerDoc (c ^. constructorName . nameId) j
  RegisterInductive ity ->
    let j = ity ^. inductiveDoc
     in do
          registerDoc (ity ^. inductiveName . nameId) j
  RegisterFunctionDef f ->
    let j = f ^. signDoc
     in do
          registerDoc (f ^. signName . nameId) j
  RegisterName n -> modify (over infoHighlightNames (cons (S.anameFromName n)))
  RegisterScopedIden n -> modify (over infoHighlightNames (cons (anameFromScopedIden n)))
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
  GetInfoTable ->
    get

runInfoTableBuilderRepl :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilderRepl = ignoreHighlightBuilder . runInfoTableBuilder . raiseUnder

runInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder = runState mempty . toState

ignoreInfoTableBuilder :: Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = evalState mempty . toState

anameFromScopedIden :: ScopedIden -> AName
anameFromScopedIden s =
  AName
    { _anameLoc = getLoc s,
      _anameKind = getNameKind s,
      _anameDocId = s ^. scopedIdenFinal . nameId,
      _anameDefinedLoc = s ^. scopedIdenName . nameDefined,
      _anameVerbatim = s ^. scopedIdenName . nameVerbatim
    }

lookupInfo :: (Members '[InfoTableBuilder, Reader InfoTable] r) => (InfoTable -> Maybe a) -> Sem r a
lookupInfo f = do
  tab1 <- ask
  fromMaybe (fromJust (f tab1)) . f <$> getInfoTable

lookupFixity :: (Members '[InfoTableBuilder, Reader InfoTable] r) => S.NameId -> Sem r FixityDef
lookupFixity uid = lookupInfo (HashMap.lookup uid . (^. infoFixities))
