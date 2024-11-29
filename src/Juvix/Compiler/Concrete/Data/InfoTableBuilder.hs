module Juvix.Compiler.Concrete.Data.InfoTableBuilder where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Data.Highlight.Builder
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
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
  RegisterLocalModule :: ScopedModule -> InfoTableBuilder m ()
  GetBuilderInfoTable :: InfoTableBuilder m InfoTable
  GetBuiltinSymbol' :: Interval -> BuiltinPrim -> InfoTableBuilder m S.Symbol
  RegisterBuiltin' :: BuiltinPrim -> S.Symbol -> InfoTableBuilder m ()

makeSem ''InfoTableBuilder

registerBuiltin :: (IsBuiltin a, Member InfoTableBuilder r) => a -> S.Symbol -> Sem r ()
registerBuiltin b sym = registerBuiltin' (toBuiltinPrim b) sym

getBuiltinSymbol :: (IsBuiltin a, Member InfoTableBuilder r) => Interval -> a -> Sem r S.Symbol
getBuiltinSymbol i = getBuiltinSymbol' i . toBuiltinPrim

evalInfoTableBuilder :: (Members '[Error ScoperError, HighlightBuilder] r) => InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r a
evalInfoTableBuilder ini = fmap snd . runInfoTableBuilder ini

runInfoTableBuilder :: (Members '[Error ScoperError, HighlightBuilder] r) => InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilder ini = reinterpret (runState ini) $ \case
  RegisterAxiom d ->
    let j = d ^. axiomDoc
     in do
          modify' (over infoAxioms (HashMap.insert (d ^. axiomName . nameId) d))
          highlightDoc (d ^. axiomName . nameId) j
  RegisterConstructor c ->
    let j = c ^. constructorDoc
     in do
          modify' (over infoConstructors (HashMap.insert (c ^. constructorName . nameId) c))
          highlightDoc (c ^. constructorName . nameId) j
  RegisterInductive ity ->
    let j = ity ^. inductiveDoc
     in do
          modify' (over infoInductives (HashMap.insert (ity ^. inductiveName . nameId) ity))
          highlightDoc (ity ^. inductiveName . nameId) j
  RegisterFunctionDef f -> do
    let j = f ^. signDoc
        fid = f ^. functionDefName . functionDefNameScoped . nameId
    modify' (over infoFunctions (HashMap.insert fid f))
    highlightDoc fid j
  RegisterName n -> highlightName (S.anameFromName n)
  RegisterScopedIden n -> highlightName (anameFromScopedIden n)
  RegisterModuleDoc uid doc -> highlightDoc uid doc
  RegisterFixity f -> do
    let sid = f ^. fixityDefSymbol . S.nameId
    modify (over infoFixities (HashMap.insert sid f))
    case f ^. fixityDefFixity . fixityId of
      Just fid -> modify (over infoPrecedenceGraph (HashMap.alter (Just . fromMaybe mempty) fid))
      Nothing -> return ()
  RegisterPrecedence l h ->
    modify (over infoPrecedenceGraph (HashMap.alter (Just . HashSet.insert h . fromMaybe mempty) l))
  RegisterHighlightDoc fid doc ->
    highlightDoc fid doc
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
  RegisterLocalModule m ->
    mapM_ (uncurry registerBuiltinHelper) (m ^. scopedModuleInfoTable . infoBuiltins . to HashMap.toList)
  GetBuilderInfoTable ->
    get
  GetBuiltinSymbol' i b -> do
    tbl <- get @InfoTable
    mapError ErrBuiltinNotDefined
      . runReader (tbl ^. infoBuiltins)
      $ getBuiltinSymbolHelper i b
  RegisterBuiltin' b n -> registerBuiltinHelper b n

getBuiltinSymbolHelper ::
  forall r.
  ( Members
      '[Error BuiltinNotDefined, Reader BuiltinsTable]
      r
  ) =>
  Interval ->
  BuiltinPrim ->
  Sem r S.Symbol
getBuiltinSymbolHelper i b =
  fromMaybeM notDefined (asks @BuiltinsTable (^. at b))
  where
    notDefined :: forall r' x. (Members '[Error BuiltinNotDefined] r') => Sem r' x
    notDefined =
      throw $
        BuiltinNotDefined
          { _notDefinedBuiltin = b,
            _notDefinedLoc = i
          }

registerBuiltinHelper :: (Members '[Error ScoperError, State InfoTable] r) => BuiltinPrim -> S.Symbol -> Sem r ()
registerBuiltinHelper b n = do
  s <- gets (^. infoBuiltins . at b)
  case s of
    Nothing -> modify (set (infoBuiltins . at b) (Just n))
    Just {} -> alreadyDefined
  where
    alreadyDefined :: forall r' x. (Members '[Error ScoperError] r') => Sem r' x
    alreadyDefined =
      throw $
        ErrBuiltinAlreadyDefined
          BuiltinAlreadyDefined
            { _builtinAlreadyDefined = b,
              _builtinAlreadyDefinedLoc = getLoc n
            }

runInfoTableBuilderRepl :: (Members '[Error ScoperError] r) => InfoTable -> Sem (InfoTableBuilder ': r) a -> Sem r (InfoTable, a)
runInfoTableBuilderRepl tab = ignoreHighlightBuilder . runInfoTableBuilder tab . raiseUnder

ignoreInfoTableBuilder :: (Members '[Error ScoperError, HighlightBuilder] r) => Sem (InfoTableBuilder ': r) a -> Sem r a
ignoreInfoTableBuilder = fmap snd . runInfoTableBuilder mempty

anameFromScopedIden :: ScopedIden -> AName
anameFromScopedIden s =
  AName
    { _anameLoc = getLoc s,
      _anameKindPretty = getNameKindPretty s,
      _anameDocId = s ^. scopedIdenFinal . nameId,
      _anameDefinedLoc = s ^. scopedIdenSrcName . nameDefined,
      _anameVerbatim = s ^. scopedIdenSrcName . nameVerbatim
    }

lookupInfo :: (Members '[InfoTableBuilder, Reader InfoTable] r) => (InfoTable -> Maybe a) -> Sem r a
lookupInfo f = fromJust <$> lookupInfo' f

lookupInfo' :: (Members '[InfoTableBuilder, Reader InfoTable] r) => (InfoTable -> Maybe a) -> Sem r (Maybe a)
lookupInfo' f = do
  tab1 <- getBuilderInfoTable
  tab2 <- ask
  return (f tab1 <|> f tab2)

lookupFixity :: (Members '[InfoTableBuilder, Reader InfoTable] r) => S.NameId -> Sem r FixityDef
lookupFixity uid = lookupInfo (^. infoFixities . at uid)

getPrecedenceGraph :: (Members '[InfoTableBuilder, Reader InfoTable] r) => Sem r PrecedenceGraph
getPrecedenceGraph = do
  tab <- ask
  tab' <- getBuilderInfoTable
  return $ combinePrecedenceGraphs (tab ^. infoPrecedenceGraph) (tab' ^. infoPrecedenceGraph)
