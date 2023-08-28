module Juvix.Compiler.Concrete.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IntSet qualified as IntSet
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data InfoTableBuilder m a where
  RegisterAxiom :: AxiomDef 'Scoped -> InfoTableBuilder m ()
  RegisterConstructor :: S.Symbol -> ConstructorDef 'Scoped -> InfoTableBuilder m ()
  RegisterInductive :: InductiveDef 'Scoped -> InfoTableBuilder m ()
  RegisterFunctionDef :: FunctionDef 'Scoped -> InfoTableBuilder m ()
  RegisterName :: (HasLoc c) => S.Name' c -> InfoTableBuilder m ()
  RegisterScopedIden :: ScopedIden -> InfoTableBuilder m ()
  RegisterModule :: Module 'Scoped 'ModuleTop -> InfoTableBuilder m ()
  RegisterFixity :: FixityDef -> InfoTableBuilder m ()
  RegisterPrecedence :: S.NameId -> S.NameId -> InfoTableBuilder m ()
  RegisterHighlightDoc :: S.NameId -> Maybe (Judoc 'Scoped) -> InfoTableBuilder m ()
  GetInfoTable :: InfoTableBuilder m InfoTable

makeSem ''InfoTableBuilder

registerDoc :: (Members '[HighlightBuilder] r) => NameId -> Maybe (Judoc 'Scoped) -> Sem r ()
registerDoc k md = modify (set (highlightDoc . at k) md)

toState :: (Members '[HighlightBuilder] r) => Sem (InfoTableBuilder ': r) a -> Sem (State InfoTable ': r) a
toState = reinterpret $ \case
  RegisterAxiom d ->
    let ref = d ^. axiomName . S.nameId
        info = AxiomInfo d
        j = d ^. axiomDoc
     in do
          modify (over infoAxioms (HashMap.insert ref info))
          registerDoc (d ^. axiomName . nameId) j
  RegisterConstructor ind c ->
    let ref = c ^. constructorName . S.nameId
        info = ConstructorInfo c ind
        j = c ^. constructorDoc
     in do
          modify (over infoConstructors (HashMap.insert ref info))
          registerDoc (c ^. constructorName . nameId) j
  RegisterInductive ity ->
    let ref = ity ^. inductiveName . S.nameId
        info = InductiveInfo {_inductiveInfoDef = ity}
        j = ity ^. inductiveDoc
     in do
          modify (over infoInductives (HashMap.insert ref info))
          registerDoc (ity ^. inductiveName . nameId) j
  RegisterFunctionDef f ->
    let ref = f ^. signName . S.nameId
        info = FunctionInfo f
        j = f ^. signDoc
     in do
          modify (set (infoFunctions . at ref) (Just info))
          registerDoc (f ^. signName . nameId) j
  RegisterName n -> modify (over highlightNames (cons (S.anameFromName n)))
  RegisterScopedIden n -> modify (over highlightNames (cons (anameFromScopedIden n)))
  RegisterModule m -> do
    let j = m ^. moduleDoc
    modify (over infoModules (HashMap.insert (m ^. modulePath) m))
    registerDoc (m ^. modulePath . nameId) j
  RegisterFixity f -> do
    let sid = f ^. fixityDefSymbol . S.nameId
    modify (over infoFixities (HashMap.insert sid f))
    modify (over infoPriorities (IntSet.insert (f ^. fixityDefPrec)))
    case f ^. fixityDefFixity . fixityId of
      Just fid -> modify (over infoPrecedenceGraph (HashMap.alter (Just . fromMaybe mempty) fid))
      Nothing -> return ()
  RegisterPrecedence l h ->
    modify (over infoPrecedenceGraph (HashMap.alter (Just . HashSet.insert h . fromMaybe mempty) l))
  RegisterHighlightDoc fid doc ->
    registerDoc fid doc
  GetInfoTable ->
    get

runInfoTableBuilderRepl :: InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilderRepl tab = ignoreHighlightBuilder . runInfoTableBuilder tab . raiseUnder

runInfoTableBuilder :: (Members '[HighlightBuilder] r) => InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder tab = runState tab . toState

ignoreInfoTableBuilder :: (Members '[HighlightBuilder] r) => Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = evalState emptyInfoTable . toState

anameFromScopedIden :: ScopedIden -> AName
anameFromScopedIden s =
  AName
    { _anameLoc = getLoc s,
      _anameKind = getNameKind s,
      _anameDocId = s ^. scopedIdenFinal . nameId,
      _anameDefinedLoc = s ^. scopedIdenName . nameDefined,
      _anameVerbatim = s ^. scopedIdenName . nameVerbatim
    }
