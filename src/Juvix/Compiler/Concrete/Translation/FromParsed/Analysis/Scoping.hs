module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error,
  )
where

import Control.Monad.Combinators.Expr qualified as P
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Base (maxInt, minInt)
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.InfoTableBuilder
import Juvix.Compiler.Concrete.Data.Name qualified as N
import Juvix.Compiler.Concrete.Data.NameSignature
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra (recordNameSignatureByIndex)
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Gen qualified as G
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty (ppTrace)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context (ParserResult)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.FixityInfo qualified as FI
import Juvix.Data.NameKind
import Juvix.Prelude hiding (scoped)

iniScoperState :: ScoperState
iniScoperState =
  ScoperState
    { _scoperModulesCache = ModulesCache mempty,
      _scoperModules = mempty,
      _scoperScope = mempty,
      _scoperSignatures = mempty,
      _scoperRecordFields = mempty,
      _scoperAlias = mempty,
      _scoperConstructorFields = mempty
    }

scopeCheck ::
  (Members '[HighlightBuilder, Error ScoperError, NameIdGen, Reader EntryPoint] r) =>
  ParserResult ->
  NonEmpty (Module 'Parsed 'ModuleTop) ->
  Sem r ScoperResult
scopeCheck pr modules =
  fmap mkResult
    . runInfoTableBuilder emptyInfoTable
    . runReader iniScopeParameters
    . runState iniScoperState
    $ checkTopModules modules
  where
    iniScopeParameters :: ScopeParameters
    iniScopeParameters =
      ScopeParameters
        { _scopeTopParents = mempty,
          _scopeParsedModules = pr ^. Parsed.resultTable . Parsed.infoParsedModules
        }
    mkResult :: (InfoTable, (ScoperState, (NonEmpty (Module 'Scoped 'ModuleTop), HashSet NameId))) -> ScoperResult
    mkResult (st, (scoperSt, (ms, exp))) =
      ScoperResult
        { _resultParserResult = pr,
          _resultScoperTable = st,
          _resultModules = ms,
          _resultExports = exp,
          _resultScope = scoperSt ^. scoperScope,
          _resultScoperState = scoperSt
        }

-- TODO refactor to have less code duplication
scopeCheckExpressionAtoms ::
  forall r.
  (Members '[Error JuvixError, NameIdGen, State Scope] r) =>
  InfoTable ->
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
scopeCheckExpressionAtoms tab as = mapError (JuvixError @ScoperError) $ do
  fmap snd
    . ignoreHighlightBuilder
    . runInfoTableBuilder tab
    . runReader iniScopeParameters
    . evalState iniScoperState
    . withLocalScope
    $ checkExpressionAtoms as
  where
    iniScopeParameters :: ScopeParameters
    iniScopeParameters =
      ScopeParameters
        { _scopeTopParents = mempty,
          _scopeParsedModules = mempty
        }

scopeCheckExpression ::
  forall r.
  (Members '[Error JuvixError, NameIdGen, State Scope, State ScoperState] r) =>
  InfoTable ->
  ExpressionAtoms 'Parsed ->
  Sem r Expression
scopeCheckExpression tab as = mapError (JuvixError @ScoperError) $ do
  fmap snd
    . ignoreHighlightBuilder
    . runInfoTableBuilder tab
    . runReader iniScopeParameters
    . withLocalScope
    $ checkParseExpressionAtoms as
  where
    iniScopeParameters :: ScopeParameters
    iniScopeParameters =
      ScopeParameters
        { _scopeTopParents = mempty,
          _scopeParsedModules = mempty
        }

scopeCheckImport ::
  forall r.
  (Members '[Error JuvixError, InfoTableBuilder, NameIdGen, State Scope, Reader ScopeParameters, State ScoperState] r) =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
scopeCheckImport = mapError (JuvixError @ScoperError) . checkImport

scopeCheckOpenModule ::
  forall r.
  (Members '[Error JuvixError, InfoTableBuilder, NameIdGen, State Scope, Reader ScopeParameters, State ScoperState] r) =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
scopeCheckOpenModule = mapError (JuvixError @ScoperError) . checkOpenModule

freshVariable :: (Members '[NameIdGen, State ScoperSyntax, State Scope, State ScoperState] r) => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol KNameLocal

checkProjectionDef ::
  forall r.
  (Members '[Error ScoperError, InfoTableBuilder, Reader BindingStrategy, State Scope, State ScoperState, NameIdGen, State ScoperSyntax] r) =>
  ProjectionDef 'Parsed ->
  Sem r (ProjectionDef 'Scoped)
checkProjectionDef p = do
  _projectionField <- getReservedDefinitionSymbol (p ^. projectionField)
  return
    ProjectionDef
      { _projectionFieldIx = p ^. projectionFieldIx,
        _projectionConstructor = p ^. projectionConstructor,
        _projectionField
      }

freshSymbol ::
  forall r.
  (Members '[State Scope, State ScoperState, NameIdGen, State ScoperSyntax] r) =>
  NameKind ->
  Symbol ->
  Sem r S.Symbol
freshSymbol _nameKind _nameConcrete = do
  _nameId <- freshNameId
  _nameDefinedIn <- gets (^. scopePath)
  let _nameDefined = getLoc _nameConcrete
      _nameWhyInScope = S.BecauseDefined
      _nameVisibilityAnn = VisPublic
      _nameVerbatim = _nameConcrete ^. symbolText
  _nameFixity <- fixity
  _nameIterator <- iter
  return S.Name' {..}
  where
    fixity ::
      Sem r (Maybe Fixity)
    fixity
      | S.canHaveFixity _nameKind = do
          mf <- gets (^? scoperSyntaxOperators . scoperOperators . at _nameConcrete . _Just . symbolOperatorFixity)
          when (isJust mf) (modify (set (scoperSyntaxOperators . scoperOperators . at _nameConcrete . _Just . symbolOperatorUsed) True))
          return mf
      | otherwise = return Nothing

    iter :: Sem r (Maybe IteratorInfo)
    iter
      | S.canBeIterator _nameKind = do
          mma :: Maybe (Maybe ParsedIteratorInfo) <- gets (^? scoperSyntaxIterators . scoperIterators . at _nameConcrete . _Just . symbolIteratorDef . iterInfo)
          runFail $ do
            ma <- failMaybe mma
            modify (set (scoperSyntaxIterators . scoperIterators . at _nameConcrete . _Just . symbolIteratorUsed) True)
            return (maybe emptyIteratorInfo fromParsedIteratorInfo ma)
      | otherwise = return Nothing

reserveSymbolSignatureOf ::
  forall (k :: NameKind) r d.
  ( Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r,
    HasNameSignature d,
    SingI (NameKindNameSpace k)
  ) =>
  Sing k ->
  d ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolSignatureOf k d s = do
  sig <- mkNameSignature d
  reserveSymbolOf k (Just sig) s

reserveSymbolOf ::
  forall (nameKind :: NameKind) (ns :: NameSpace) r.
  ( Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r,
    ns ~ NameKindNameSpace nameKind,
    SingI ns
  ) =>
  Sing nameKind ->
  Maybe NameSignature ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolOf k nameSig s = do
  checkNotBound
  path <- gets (^. scopePath)
  strat <- ask
  s' <- freshSymbol (fromSing k) s
  whenJust nameSig (modify' . set (scoperSignatures . at (s' ^. S.nameId)) . Just)
  modify (set (scopeNameSpaceLocal sns . at s) (Just s'))
  registerName s'
  let u = S.unConcrete s'
      entry :: NameSpaceEntryType (NameKindNameSpace nameKind)
      entry =
        let symE
              | isAlias = PreSymbolAlias (Alias u)
              | otherwise = PreSymbolFinal (SymbolEntry u)
            modE = ModuleSymbolEntry u
            fixE = FixitySymbolEntry u
         in case k of
              SKNameConstructor -> symE
              SKNameAlias -> symE
              SKNameInductive -> symE
              SKNameFunction -> symE
              SKNameAxiom -> symE
              SKNameLocal -> symE
              SKNameLocalModule -> modE
              SKNameTopModule -> modE
              SKNameFixity -> fixE
      addS :: NameSpaceEntryType ns -> Maybe (SymbolInfo ns) -> SymbolInfo ns
      addS mentry m = case m of
        Nothing -> symbolInfoSingle mentry
        Just SymbolInfo {..} -> case strat of
          BindingLocal -> symbolInfoSingle mentry
          BindingTop -> SymbolInfo (HashMap.insert path mentry _symbolInfo)
  modify (over scopeNameSpace (HashMap.alter (Just . addS entry) s))
  return s'
  where
    isAlias = case k of
      SKNameAlias -> True
      _ -> False
    sns :: Sing ns = sing
    checkNotBound :: Sem r ()
    checkNotBound = do
      exists <- HashMap.lookup s <$> gets (^. scopeNameSpaceLocal sns)
      whenJust exists $ \d ->
        throw
          ( ErrMultipleDeclarations
              MultipleDeclarations
                { _multipleDeclSecond = s,
                  _multipleDeclFirst = getLoc d
                }
          )

getReservedDefinitionSymbol ::
  forall r.
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r) =>
  Symbol ->
  Sem r S.Symbol
getReservedDefinitionSymbol s = do
  m <- gets (^. scopeLocalSymbols)
  let s' = fromMaybe err (m ^. at s)
      err = error ("impossible. Contents of scope:\n" <> ppTrace (toList m))
  return s'

ignoreSyntax :: Sem (State ScoperSyntax ': r) a -> Sem r a
ignoreSyntax = evalState emptyScoperSyntax

-- | Variables are assumed to never be infix operators
bindVariableSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder, State ScoperState] r) =>
  Symbol ->
  Sem r S.Symbol
bindVariableSymbol = localBindings . ignoreSyntax . reserveSymbolOf SKNameLocal Nothing

reserveInductiveSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r) =>
  InductiveDef 'Parsed ->
  Sem r S.Symbol
reserveInductiveSymbol d = reserveSymbolSignatureOf SKNameInductive d (d ^. inductiveName)

reserveAliasSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, Reader BindingStrategy, InfoTableBuilder, State ScoperState] r) =>
  Symbol ->
  Sem r S.Symbol
reserveAliasSymbol = reserveSymbolOf SKNameAlias Nothing

reserveProjectionSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, Reader BindingStrategy, InfoTableBuilder, State ScoperState] r) =>
  ProjectionDef 'Parsed ->
  Sem r S.Symbol
reserveProjectionSymbol d = reserveSymbolOf SKNameFunction Nothing (d ^. projectionField)

reserveConstructorSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r) =>
  InductiveDef 'Parsed ->
  ConstructorDef 'Parsed ->
  Sem r S.Symbol
reserveConstructorSymbol d c = reserveSymbolSignatureOf SKNameConstructor (d, c) (c ^. constructorName)

reserveFunctionSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r) =>
  FunctionDef 'Parsed ->
  Sem r S.Symbol
reserveFunctionSymbol f =
  reserveSymbolSignatureOf SKNameFunction f (f ^. signName)

reserveAxiomSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r) =>
  AxiomDef 'Parsed ->
  Sem r S.Symbol
reserveAxiomSymbol a = reserveSymbolSignatureOf SKNameAxiom a (a ^. axiomName)

bindFunctionSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r) =>
  Symbol ->
  Sem r S.Symbol
bindFunctionSymbol = getReservedDefinitionSymbol

bindInductiveSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r) =>
  Symbol ->
  Sem r S.Symbol
bindInductiveSymbol = getReservedDefinitionSymbol

bindAxiomSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r) =>
  Symbol ->
  Sem r S.Symbol
bindAxiomSymbol = getReservedDefinitionSymbol

bindConstructorSymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r) =>
  Symbol ->
  Sem r S.Symbol
bindConstructorSymbol = getReservedDefinitionSymbol

bindFixitySymbol ::
  (Members '[Error ScoperError, NameIdGen, State ScoperSyntax, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r) =>
  Symbol ->
  Sem r S.Symbol
bindFixitySymbol s = do
  m <- gets (^. scopeLocalFixitySymbols)
  let s' = fromMaybe err (m ^. at s)
      err = error ("impossible. Contents of scope:\n" <> ppTrace (toList m))
  return s'

checkImport ::
  forall r.
  (Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImport import_@(Import kw path qual) = do
  checkCycle
  cache <- gets (^. scoperModulesCache . cachedModules)
  moduleRef <- maybe (readScopeModule import_) return (cache ^. at path)
  let checked :: Module 'Scoped 'ModuleTop = moduleRef ^. moduleRefModule
      sname :: S.TopModulePath = checked ^. modulePath
      sname' :: S.Name = set S.nameConcrete (topModulePathToName path) sname
      moduleId = sname ^. S.nameId
      cmoduleRef :: ModuleRef'' 'S.Concrete 'ModuleTop = set moduleRefName sname' moduleRef
      importName :: S.TopModulePath = set S.nameConcrete path sname
      synonymName :: Maybe S.TopModulePath = do
        synonym <- qual
        return (set S.nameConcrete synonym sname)
      qual' :: Maybe S.TopModulePath
      qual' = do
        asName <- qual
        return (set S.nameConcrete asName sname')
  addModuleToScope moduleRef
  registerName importName
  whenJust synonymName registerName
  let moduleRef' = mkModuleRef' moduleRef
  modify (over scoperModules (HashMap.insert moduleId moduleRef'))
  return (Import kw cmoduleRef qual')
  where
    addModuleToScope :: ModuleRef'' 'S.NotConcrete 'ModuleTop -> Sem r ()
    addModuleToScope moduleRef = do
      let mpath :: TopModulePath = fromMaybe path qual
          uid :: S.NameId = moduleRef ^. moduleRefName . S.nameId
          singTbl = HashMap.singleton uid moduleRef
      modify (over (scopeTopModules . at mpath) (Just . maybe singTbl (HashMap.insert uid moduleRef)))
    checkCycle :: Sem r ()
    checkCycle = do
      topp <- asks (^. scopeTopParents)
      case span (/= import_) topp of
        (_, []) -> return ()
        (c, _) ->
          let cyc = NonEmpty.reverse (import_ :| c)
           in throw (ErrImportCycle (ImportCycle cyc))

getTopModulePath :: Module 'Parsed 'ModuleTop -> S.AbsModulePath
getTopModulePath Module {..} =
  S.AbsModulePath
    { S._absTopModulePath = _modulePath,
      S._absLocalPath = mempty
    }

getModuleExportInfo :: forall r. (Members '[State ScoperState] r) => ModuleSymbolEntry -> Sem r ExportInfo
getModuleExportInfo m = fromMaybeM err (gets (^? scoperModules . at (m ^. moduleEntry . S.nameId) . _Just . to getModuleRefExportInfo))
  where
    err :: Sem r a
    err = do
      ms <- toList <$> gets (^. scoperModules)
      error
        ( "impossible. Could not find "
            <> ppTrace m
            <> "\nModules in the state: "
            <> ppTrace ms
        )

-- | Do not call directly. Looks for a symbol in (possibly) nested local modules
lookupSymbolAux ::
  forall r.
  (Members '[State ScoperState, State Scope, Output ModuleSymbolEntry, Output PreSymbolEntry, Output FixitySymbolEntry] r) =>
  [Symbol] ->
  Symbol ->
  Sem r ()
lookupSymbolAux modules final = do
  hereOrInLocalModule
  importedTopModule
  where
    hereOrInLocalModule :: Sem r () =
      case modules of
        [] -> do
          let helper ::
                forall ns r'.
                (SingI ns, Members '[Output (NameSpaceEntryType ns), State Scope] r') =>
                Proxy ns ->
                Sem r' ()
              helper Proxy =
                gets (^.. scopeNameSpace @ns . at final . _Just . symbolInfo . each) >>= mapM_ output
          helper (Proxy @'NameSpaceSymbols)
          helper (Proxy @'NameSpaceModules)
          helper (Proxy @'NameSpaceFixities)
        p : ps ->
          gets (^.. scopeModuleSymbols . at p . _Just . symbolInfo . each)
            >>= mapM_ (getModuleExportInfo >=> lookInExport final ps)
    importedTopModule :: Sem r ()
    importedTopModule = do
      tbl <- gets (^. scopeTopModules)
      mapM_ output (tbl ^.. at path . _Just . each . to (mkModuleEntry . mkModuleRef'))
      where
        path = TopModulePath modules final

mkModuleEntry :: ModuleRef' 'S.NotConcrete -> ModuleSymbolEntry
mkModuleEntry (ModuleRef' (t :&: m)) = ModuleSymbolEntry $ case t of
  SModuleTop -> S.unConcrete (m ^. moduleRefModule . modulePath)
  SModuleLocal -> S.unConcrete (m ^. moduleRefModule . modulePath)

lookInExport ::
  forall r.
  (Members '[State ScoperState, Output PreSymbolEntry, Output ModuleSymbolEntry, Output FixitySymbolEntry] r) =>
  Symbol ->
  [Symbol] ->
  ExportInfo ->
  Sem r ()
lookInExport sym remaining e = case remaining of
  [] -> do
    whenJust (e ^. exportSymbols . at sym) output
    whenJust (e ^. exportModuleSymbols . at sym) output
    whenJust (e ^. exportFixitySymbols . at sym) output
  s : ss -> whenJustM (mayModule e s) (lookInExport sym ss)
  where
    mayModule :: ExportInfo -> Symbol -> Sem r (Maybe ExportInfo)
    mayModule ExportInfo {..} s =
      mapM getModuleExportInfo (HashMap.lookup s _exportModuleSymbols)

-- | We return a list of entries because qualified names can point to different
-- modules due to nesting.
lookupQualifiedSymbol ::
  forall r.
  (Members '[State Scope, State ScoperState] r) =>
  ([Symbol], Symbol) ->
  Sem r ([PreSymbolEntry], [ModuleSymbolEntry], [FixitySymbolEntry])
lookupQualifiedSymbol sms = do
  (es, (ms, fs)) <- runOutputList . runOutputList . execOutputList $ go sms
  return (es, ms, fs)
  where
    go ::
      forall r'.
      (Members '[State ScoperState, State Scope, Output PreSymbolEntry, Output ModuleSymbolEntry, Output FixitySymbolEntry] r') =>
      ([Symbol], Symbol) ->
      Sem r' ()
    go (path, sym) = do
      here
      there
      where
        -- Current module.
        here :: Sem r' ()
        here = lookupSymbolAux path sym
        -- Looks for a top level modules
        there :: Sem r' ()
        there = mapM_ (uncurry lookInTopModule) allTopPaths
          where
            allTopPaths :: [(TopModulePath, [Symbol])]
            allTopPaths = map (first nonEmptyToTopPath) raw
              where
                lpath = toList path
                raw :: [(NonEmpty Symbol, [Symbol])]
                raw =
                  [ (l, r) | i <- [1 .. length path], (Just l, r) <- [first nonEmpty (splitAt i lpath)]
                  ]
                nonEmptyToTopPath :: NonEmpty Symbol -> TopModulePath
                nonEmptyToTopPath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)
            lookInTopModule :: TopModulePath -> [Symbol] -> Sem r' ()
            lookInTopModule topPath remaining = do
              tbl <- gets (^. scopeTopModules)
              sequence_
                [ lookInExport sym remaining (ref ^. moduleExportInfo)
                  | Just t <- [tbl ^. at topPath],
                    ref <- toList t
                ]

-- | This assumes that alias do not have cycles.
normalizePreSymbolEntry :: (Members '[State ScoperState] r) => PreSymbolEntry -> Sem r SymbolEntry
normalizePreSymbolEntry = \case
  PreSymbolFinal a -> return a
  PreSymbolAlias a -> gets (^?! scoperAlias . at (a ^. aliasName . S.nameId) . _Just) >>= normalizePreSymbolEntry

checkQualifiedName ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  QualifiedName ->
  Sem r PreSymbolEntry
checkQualifiedName q@(QualifiedName (SymbolPath p) sym) = do
  es <- fst3 <$> lookupQualifiedSymbol (toList p, sym)
  case es of
    [] -> notInScope
    [e] -> return e
    _ -> throw (ErrAmbiguousSym (AmbiguousSym q' es))
  where
    q' = NameQualified q
    notInScope = throw (ErrQualSymNotInScope (QualSymNotInScope q))

entryToScopedIden ::
  (Members '[InfoTableBuilder, State ScoperState] r) =>
  Name ->
  PreSymbolEntry ->
  Sem r ScopedIden
entryToScopedIden name e = do
  let helper :: S.Name' x -> S.Name
      helper = set S.nameConcrete name
      scopedName :: S.Name
      scopedName = helper (e ^. preSymbolName)
  si <- case e of
    PreSymbolFinal {} ->
      return
        ScopedIden
          { _scopedIdenFinal = scopedName,
            _scopedIdenAlias = Nothing
          }
    PreSymbolAlias {} -> do
      e' <- normalizePreSymbolEntry e
      let scopedName' =
            over S.nameFixity (maybe (e' ^. symbolEntry . S.nameFixity) Just) $
              set S.nameKind (getNameKind e') scopedName
      return
        ScopedIden
          { _scopedIdenAlias = Just scopedName',
            _scopedIdenFinal = helper (e' ^. symbolEntry)
          }
  registerScopedIden si
  return si

-- | We gather all symbols which have been defined or marked to be public in the given scope.
exportScope ::
  forall r.
  (Members '[State Scope, Error ScoperError] r) =>
  Scope ->
  Sem r ExportInfo
exportScope Scope {..} = do
  _exportSymbols <- HashMap.fromList <$> mapMaybeM mkentry (HashMap.toList _scopeSymbols)
  _exportModuleSymbols <- HashMap.fromList <$> mapMaybeM mkentry (HashMap.toList _scopeModuleSymbols)
  _exportFixitySymbols <- HashMap.fromList <$> mapMaybeM mkentry (HashMap.toList _scopeFixitySymbols)
  return ExportInfo {..}
  where
    mkentry ::
      forall ns.
      (SingI ns) =>
      (Symbol, SymbolInfo ns) ->
      Sem r (Maybe (Symbol, NameSpaceEntryType ns))
    mkentry (s, SymbolInfo {..}) =
      case filter shouldExport (toList _symbolInfo) of
        [] -> return Nothing
        [e] -> return (Just (s, e))
        e : es -> err (e :| es)
      where
        shouldExport :: NameSpaceEntryType ns -> Bool
        shouldExport ent = ent ^. nsEntry . S.nameVisibilityAnn == VisPublic

        err :: NonEmpty (NameSpaceEntryType ns) -> Sem r a
        err es =
          throw
            ( ErrMultipleExport
                ( MultipleExportConflict
                    _scopePath
                    s
                    ( case sing :: SNameSpace ns of
                        SNameSpaceSymbols -> ExportEntriesSymbols es
                        SNameSpaceModules -> ExportEntriesModules es
                        SNameSpaceFixities -> ExportEntriesFixities es
                    )
                )
            )

getParsedModule :: (Members '[Reader ScopeParameters] r) => TopModulePath -> Sem r (Module 'Parsed 'ModuleTop)
getParsedModule i = asks (^?! scopeParsedModules . at i . _Just)

readScopeModule ::
  (Members '[Error ScoperError, Reader ScopeParameters, NameIdGen, State ScoperState, InfoTableBuilder] r) =>
  Import 'Parsed ->
  Sem r (ModuleRef'' 'S.NotConcrete 'ModuleTop)
readScopeModule import_ = do
  m <- getParsedModule (import_ ^. importModule)
  local addImport (checkTopModule m)
  where
    addImport :: ScopeParameters -> ScopeParameters
    addImport = over scopeTopParents (cons import_)

checkFixityInfo ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, State ScoperSyntax, NameIdGen, InfoTableBuilder] r) =>
  ParsedFixityInfo 'Parsed ->
  Sem r (ParsedFixityInfo 'Scoped)
checkFixityInfo ParsedFixityInfo {..} = do
  fields' <- mapM checkFields _fixityFields
  return
    ParsedFixityInfo
      { _fixityFields = fields',
        ..
      }
  where
    checkFields :: ParsedFixityFields 'Parsed -> Sem r (ParsedFixityFields 'Scoped)
    checkFields ParsedFixityFields {..} = do
      same' <- mapM checkFixitySymbol _fixityFieldsPrecSame
      below' <- mapM (mapM checkFixitySymbol) _fixityFieldsPrecBelow
      above' <- mapM (mapM checkFixitySymbol) _fixityFieldsPrecAbove
      return
        ParsedFixityFields
          { _fixityFieldsPrecSame = same',
            _fixityFieldsPrecAbove = above',
            _fixityFieldsPrecBelow = below',
            _fixityFieldsAssoc,
            _fixityFieldsBraces
          }

checkFixitySyntaxDef ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, State ScoperSyntax, NameIdGen, InfoTableBuilder] r) =>
  FixitySyntaxDef 'Parsed ->
  Sem r (FixitySyntaxDef 'Scoped)
checkFixitySyntaxDef FixitySyntaxDef {..} = topBindings $ do
  sym <- bindFixitySymbol _fixitySymbol
  doc <- mapM checkJudoc _fixityDoc
  info' <- checkFixityInfo _fixityInfo
  registerHighlightDoc (sym ^. S.nameId) doc
  return
    FixitySyntaxDef
      { _fixitySymbol = sym,
        _fixityDoc = doc,
        _fixityInfo = info',
        _fixityAssignKw,
        _fixitySyntaxKw,
        _fixityKw
      }

resolveFixitySyntaxDef ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, State ScoperSyntax, NameIdGen, InfoTableBuilder] r) =>
  FixitySyntaxDef 'Parsed ->
  Sem r ()
resolveFixitySyntaxDef fdef@FixitySyntaxDef {..} = topBindings $ do
  sym <- reserveSymbolOf SKNameFixity Nothing _fixitySymbol
  let fi :: ParsedFixityInfo 'Parsed = _fixityInfo
  same <- mapM checkFixitySymbol (fi ^. fixityPrecSame)
  below <- mapM (mapM checkFixitySymbol) (fi ^. fixityPrecBelow)
  above <- mapM (mapM checkFixitySymbol) (fi ^. fixityPrecAbove)
  tab <- getInfoTable
  fid <- maybe freshNameId (return . getFixityId tab) same
  let below' = map (getFixityId tab) <$> below
      above' = map (getFixityId tab) <$> above
  forM_ above' $ mapM_ (`registerPrecedence` fid)
  forM_ below' $ mapM_ (registerPrecedence fid)
  let samePrec = getPrec tab <$> same
      belowPrec :: Integer
      belowPrec = fromIntegral $ maximum (minInt + 1 : maybe [] (map (getPrec tab)) above)
      abovePrec :: Integer
      abovePrec = fromIntegral $ minimum (maxInt - 1 : maybe [] (map (getPrec tab)) below)
  when (belowPrec >= abovePrec + 1) $
    throw (ErrPrecedenceInconsistency (PrecedenceInconsistencyError fdef))
  when (isJust same && not (null below && null above)) $
    throw (ErrPrecedenceInconsistency (PrecedenceInconsistencyError fdef))
  -- we need Integer to avoid overflow when computing prec
  let prec = fromMaybe (fromInteger $ (abovePrec + belowPrec) `div` 2) samePrec
      fx =
        Fixity
          { _fixityId = Just fid,
            _fixityPrecedence = PrecNat prec,
            _fixityArity =
              case fi ^. fixityParsedArity . withLocParam of
                FI.Unary -> OpUnary AssocPostfix
                FI.Binary -> case fi ^. fixityAssoc of
                  Nothing -> OpBinary AssocNone
                  Just FI.AssocLeft -> OpBinary AssocLeft
                  Just FI.AssocRight -> OpBinary AssocRight
                  Just FI.AssocNone -> OpBinary AssocNone
                FI.None -> OpNone
          }
  registerFixity
    @$> FixityDef
      { _fixityDefSymbol = sym,
        _fixityDefFixity = fx,
        _fixityDefPrec = prec
      }
  return ()
  where
    getFixityDef :: InfoTable -> S.Symbol -> FixityDef
    getFixityDef tab = fromJust . flip HashMap.lookup (tab ^. infoFixities) . (^. S.nameId)

    getPrec :: InfoTable -> S.Symbol -> Int
    getPrec tab = (^. fixityDefPrec) . getFixityDef tab

    getFixityId :: InfoTable -> S.Symbol -> S.NameId
    getFixityId tab = fromJust . (^. fixityDefFixity . fixityId) . getFixityDef tab

resolveOperatorSyntaxDef ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, State ScoperSyntax, InfoTableBuilder] r) =>
  OperatorSyntaxDef ->
  Sem r ()
resolveOperatorSyntaxDef s@OperatorSyntaxDef {..} = do
  checkNotDefined
  sym <- checkFixitySymbol _opFixity
  tab <- getInfoTable
  let fx = fromJust (HashMap.lookup (sym ^. S.nameId) (tab ^. infoFixities)) ^. fixityDefFixity
      sf =
        SymbolOperator
          { _symbolOperatorUsed = False,
            _symbolOperatorDef = s,
            _symbolOperatorFixity = fx
          }
  modify (over scoperSyntaxOperators (over scoperOperators (HashMap.insert _opSymbol sf)))
  where
    checkNotDefined :: Sem r ()
    checkNotDefined =
      whenJustM
        (HashMap.lookup _opSymbol <$> gets (^. scoperSyntaxOperators . scoperOperators))
        $ \s' -> throw (ErrDuplicateOperator (DuplicateOperator (s' ^. symbolOperatorDef) s))

resolveIteratorSyntaxDef ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, State ScoperSyntax] r) =>
  IteratorSyntaxDef ->
  Sem r ()
resolveIteratorSyntaxDef s@IteratorSyntaxDef {..} = do
  checkNotDefined
  checkAtLeastOneRange
  let sf =
        SymbolIterator
          { _symbolIteratorUsed = False,
            _symbolIteratorDef = s
          }
  modify (set (scoperSyntaxIterators . scoperIterators . at _iterSymbol) (Just sf))
  where
    checkAtLeastOneRange :: Sem r ()
    checkAtLeastOneRange = unless (maybe True (> 0) num) (throw (ErrInvalidRangeNumber (InvalidRangeNumber s)))
      where
        num :: Maybe Int
        num = s ^? iterInfo . _Just . to fromParsedIteratorInfo . iteratorInfoRangeNum . _Just

    checkNotDefined :: Sem r ()
    checkNotDefined =
      whenJustM
        (HashMap.lookup _iterSymbol <$> gets (^. scoperSyntaxIterators . scoperIterators))
        $ \s' -> throw (ErrDuplicateIterator (DuplicateIterator (s' ^. symbolIteratorDef) s))

-- | Only used as syntactical convenience for registerX functions
(@$>) :: (Functor m) => (a -> m ()) -> a -> m a
(@$>) f a = f a $> a

checkFunctionDef ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperSyntax, Reader BindingStrategy] r) =>
  FunctionDef 'Parsed ->
  Sem r (FunctionDef 'Scoped)
checkFunctionDef FunctionDef {..} = do
  sigName' <- bindFunctionSymbol _signName
  sigDoc' <- mapM checkJudoc _signDoc
  (args', sigType', sigBody') <- withLocalScope $ do
    a' <- mapM checkArg _signArgs
    t' <- checkParseExpressionAtoms _signRetType
    b' <- checkBody
    return (a', t', b')
  registerFunctionDef
    @$> FunctionDef
      { _signName = sigName',
        _signRetType = sigType',
        _signDoc = sigDoc',
        _signBody = sigBody',
        _signArgs = args',
        ..
      }
  where
    checkArg :: SigArg 'Parsed -> Sem r (SigArg 'Scoped)
    checkArg SigArg {..} = do
      names' <- forM _sigArgNames $ \case
        ArgumentSymbol s -> ArgumentSymbol <$> bindVariableSymbol s
        ArgumentWildcard w -> return $ ArgumentWildcard w
      ty' <- mapM checkParseExpressionAtoms _sigArgType
      return
        SigArg
          { _sigArgNames = names',
            _sigArgType = ty',
            ..
          }
    checkBody :: Sem r (FunctionDefBody 'Scoped)
    checkBody = case _signBody of
      SigBodyExpression e -> SigBodyExpression <$> checkParseExpressionAtoms e
      SigBodyClauses cls -> SigBodyClauses <$> mapM checkClause cls
    checkClause :: FunctionClause 'Parsed -> Sem r (FunctionClause 'Scoped)
    checkClause FunctionClause {..} = do
      (patterns', body') <- withLocalScope $ do
        p <- mapM checkParsePatternAtom _clausenPatterns
        b <- checkParseExpressionAtoms _clausenBody
        return (p, b)
      return
        FunctionClause
          { _clausenBody = body',
            _clausenPatterns = patterns',
            _clausenPipeKw,
            _clausenAssignKw
          }

checkInductiveParameters ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  InductiveParameters 'Parsed ->
  Sem r (InductiveParameters 'Scoped)
checkInductiveParameters params = do
  _inductiveParametersRhs <- mapM checkRhs (params ^. inductiveParametersRhs)
  _inductiveParametersNames <- mapM bindVariableSymbol (params ^. inductiveParametersNames)
  return InductiveParameters {..}
  where
    checkRhs :: InductiveParametersRhs 'Parsed -> Sem r (InductiveParametersRhs 'Scoped)
    checkRhs rhs = do
      let _inductiveParametersColon = rhs ^. inductiveParametersColon
      _inductiveParametersType <- checkParseExpressionAtoms (rhs ^. inductiveParametersType)
      return InductiveParametersRhs {..}

checkInductiveDef ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperSyntax, Reader BindingStrategy] r) =>
  InductiveDef 'Parsed ->
  Sem r (InductiveDef 'Scoped)
checkInductiveDef InductiveDef {..} = do
  (inductiveName', constructorNames' :: NonEmpty S.Symbol) <- topBindings $ do
    i <- bindInductiveSymbol _inductiveName
    cs <- mapM (bindConstructorSymbol . (^. constructorName)) _inductiveConstructors
    return (i, cs)
  (inductiveParameters', inductiveType', inductiveDoc', inductiveConstructors') <- withLocalScope $ do
    inductiveParameters' <- mapM checkInductiveParameters _inductiveParameters
    inductiveType' <- mapM checkParseExpressionAtoms _inductiveType
    inductiveDoc' <- mapM checkJudoc _inductiveDoc
    inductiveConstructors' <-
      nonEmpty'
        <$> sequence
          [ checkConstructorDef inductiveName' cname cdef
            | (cname, cdef) <- zipExact (toList constructorNames') (toList _inductiveConstructors)
          ]
    return (inductiveParameters', inductiveType', inductiveDoc', inductiveConstructors')
  registerInductive
    @$> InductiveDef
      { _inductiveName = inductiveName',
        _inductiveDoc = inductiveDoc',
        _inductivePragmas = _inductivePragmas,
        _inductiveParameters = inductiveParameters',
        _inductiveType = inductiveType',
        _inductiveConstructors = inductiveConstructors',
        _inductiveBuiltin,
        _inductivePositive,
        _inductiveTrait,
        _inductiveAssignKw,
        _inductiveKw
      }
  where
    -- note that the constructor name is not bound here
    checkConstructorDef :: S.Symbol -> S.Symbol -> ConstructorDef 'Parsed -> Sem r (ConstructorDef 'Scoped)
    checkConstructorDef tyName constructorName' ConstructorDef {..} = do
      doc' <- mapM checkJudoc _constructorDoc
      rhs' <- checkRhs _constructorRhs
      registerConstructor tyName
        @$> ConstructorDef
          { _constructorName = constructorName',
            _constructorRhs = rhs',
            _constructorDoc = doc',
            _constructorPragmas = _constructorPragmas,
            _constructorPipe
          }

    checkRhs :: ConstructorRhs 'Parsed -> Sem r (ConstructorRhs 'Scoped)
    checkRhs = \case
      ConstructorRhsGadt r -> ConstructorRhsGadt <$> checkGadt r
      ConstructorRhsRecord r -> ConstructorRhsRecord <$> checkRecord r
      ConstructorRhsAdt r -> ConstructorRhsAdt <$> checkAdt r

    checkRecord :: RhsRecord 'Parsed -> Sem r (RhsRecord 'Scoped)
    checkRecord RhsRecord {..} = do
      fields' <- checkFields _rhsRecordFields
      return
        RhsRecord
          { _rhsRecordFields = fields',
            _rhsRecordDelim
          }
      where
        checkFields :: NonEmpty (RecordField 'Parsed) -> Sem r (NonEmpty (RecordField 'Scoped))
        checkFields (RecordField {..} :| fs) = do
          type' <- checkParseExpressionAtoms _fieldType
          withLocalScope $ do
            name' <- bindVariableSymbol _fieldName
            let f =
                  RecordField
                    { _fieldType = type',
                      _fieldName = name',
                      ..
                    }
            case nonEmpty fs of
              Nothing -> return (pure f)
              Just fs1 -> (pure f <>) <$> checkFields fs1

    checkAdt :: RhsAdt 'Parsed -> Sem r (RhsAdt 'Scoped)
    checkAdt RhsAdt {..} = do
      args' <- mapM checkParseExpressionAtoms _rhsAdtArguments
      return
        RhsAdt
          { _rhsAdtArguments = args'
          }

    checkGadt :: RhsGadt 'Parsed -> Sem r (RhsGadt 'Scoped)
    checkGadt RhsGadt {..} = do
      constructorType' <- checkParseExpressionAtoms _rhsGadtType
      return
        RhsGadt
          { _rhsGadtType = constructorType',
            _rhsGadtColon
          }

createExportsTable :: ExportInfo -> HashSet NameId
createExportsTable = HashSet.fromList . (^.. exportAllNames . S.nameId)

checkTopModules ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  NonEmpty (Module 'Parsed 'ModuleTop) ->
  Sem r (NonEmpty (Module 'Scoped 'ModuleTop), HashSet NameId)
checkTopModules modules = do
  checked <- mapM checkTopModule modules
  return ((^. moduleRefModule) <$> checked, createExportsTable (head checked ^. moduleExportInfo))

checkTopModule_ ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule_ = fmap (^. moduleRefModule) . checkTopModule

topBindings :: Sem (Reader BindingStrategy ': r) a -> Sem r a
topBindings = runReader BindingTop

localBindings :: Sem (Reader BindingStrategy ': r) a -> Sem r a
localBindings = runReader BindingLocal

checkTopModule ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Module 'Parsed 'ModuleTop ->
  Sem r (ModuleRef'' 'S.NotConcrete 'ModuleTop)
checkTopModule m@Module {..} = do
  r <- checkedModule
  modify (over (scoperModulesCache . cachedModules) (HashMap.insert _modulePath r))
  registerModule (r ^. moduleRefModule)
  return r
  where
    freshTopModulePath ::
      forall s.
      (Members '[State ScoperState, NameIdGen, InfoTableBuilder] s) =>
      Sem s S.TopModulePath
    freshTopModulePath = do
      _nameId <- freshNameId
      let _nameDefinedIn = S.topModulePathToAbsPath _modulePath
          _nameConcrete = _modulePath
          _nameDefined = getLoc (_modulePath ^. modulePathName)
          _nameKind = KNameTopModule
          _nameFixity :: Maybe Fixity
          _nameFixity = Nothing
          -- This visibility annotation is not relevant
          _nameVisibilityAnn = VisPublic
          _nameWhyInScope = S.BecauseDefined
          _nameVerbatim = N.topModulePathToDottedPath _modulePath
          _nameIterator :: Maybe IteratorInfo
          _nameIterator = Nothing
          moduleName = S.Name' {..}
      registerName moduleName
      return moduleName

    iniScope :: Scope
    iniScope = emptyScope (getTopModulePath m)

    checkedModule :: Sem r (ModuleRef'' 'S.NotConcrete 'ModuleTop)
    checkedModule = do
      (s, (m', p)) <- runState iniScope $ do
        path' <- freshTopModulePath
        withTopScope $ do
          (_moduleExportInfo, body') <- topBindings (checkModuleBody _moduleBody)
          doc' <- mapM checkJudoc _moduleDoc
          let _moduleRefModule =
                Module
                  { _modulePath = path',
                    _moduleBody = body',
                    _moduleDoc = doc',
                    _modulePragmas = _modulePragmas,
                    _moduleKw,
                    _moduleInductive,
                    _moduleKwEnd
                  }
              _moduleRefName = S.unConcrete path'
          return (ModuleRef'' {..}, path')
      modify (set (scoperScope . at (p ^. S.nameConcrete)) (Just s))
      return m'

withTopScope :: (Members '[State Scope] r) => Sem r a -> Sem r a
withTopScope ma = do
  before <- get @Scope
  let scope' =
        ( set scopeLocalSymbols mempty
            . set scopeLocalModuleSymbols mempty
        )
          before
  put scope'
  ma

withLocalScope :: (Members '[State Scope] r) => Sem r a -> Sem r a
withLocalScope ma = do
  before <- get @Scope
  let scope' =
        ( set scopeLocalSymbols mempty
            . set scopeLocalModuleSymbols mempty
        )
          before
  put scope'
  x <- ma
  put before
  return x

syntaxBlock :: (Members '[Error ScoperError] r) => Sem (State ScoperSyntax ': r) a -> Sem r a
syntaxBlock m =
  evalState emptyScoperSyntax $ do
    a <- m
    checkOrphanOperators
    checkOrphanIterators
    return a

checkModuleBody ::
  forall r.
  (Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r) =>
  [Statement 'Parsed] ->
  Sem r (ExportInfo, [Statement 'Scoped])
checkModuleBody body = do
  body' <-
    fmap flattenSections
      . syntaxBlock
      $ checkSections (mkSections body)
  exported <- get >>= exportScope
  return (exported, body')
  where
    flattenSections :: forall s. StatementSections s -> [Statement s]
    flattenSections s = run . execOutputList $ case s of
      SectionsEmpty -> return ()
      SectionsNonDefinitions n -> goNonDefinitions n
      SectionsDefinitions n -> goDefinitions n
      where
        goNonDefinitions :: forall t. (Members '[Output (Statement s)] t) => NonDefinitionsSection s -> Sem t ()
        goNonDefinitions NonDefinitionsSection {..} = do
          mapM_ (output . toStatement) _nonDefinitionsSection
          whenJust _nonDefinitionsNext goDefinitions
          where
            toStatement :: NonDefinition s -> Statement s
            toStatement = \case
              NonDefinitionImport d -> StatementImport d
              NonDefinitionModule d -> StatementModule d
              NonDefinitionOpenModule d -> StatementOpenModule d

        goDefinitions :: forall t. (Members '[Output (Statement s)] t) => DefinitionsSection s -> Sem t ()
        goDefinitions DefinitionsSection {..} = do
          mapM_ (output . toStatement) _definitionsSection
          whenJust _definitionsNext goNonDefinitions
          where
            toStatement :: Definition s -> Statement s
            toStatement = \case
              DefinitionSyntax d -> StatementSyntax d
              DefinitionAxiom d -> StatementAxiom d
              DefinitionFunctionDef d -> StatementFunctionDef d
              DefinitionInductive d -> StatementInductive d
              DefinitionProjectionDef d -> StatementProjectionDef d

checkSections ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperSyntax] r) =>
  StatementSections 'Parsed ->
  Sem r (StatementSections 'Scoped)
checkSections sec = do
  (inductiveModules, sec' :: StatementSections 'Scoped) <- runOutputList (topBindings helper)
  return $ case (fmap NonDefinitionModule) <$> nonEmpty inductiveModules of
    Nothing -> sec'
    Just im -> case sec' of
      SectionsNonDefinitions d -> SectionsNonDefinitions (over nonDefinitionsSection (im <>) d)
      SectionsEmpty ->
        SectionsNonDefinitions
          NonDefinitionsSection
            { _nonDefinitionsSection = im,
              _nonDefinitionsNext = Nothing
            }
      SectionsDefinitions d ->
        SectionsNonDefinitions
          NonDefinitionsSection
            { _nonDefinitionsSection = im,
              _nonDefinitionsNext = Just d
            }
  where
    helper ::
      forall r'.
      (r' ~ Reader BindingStrategy ': Output (Module 'Scoped 'ModuleLocal) ': r) =>
      Sem r' (StatementSections 'Scoped)
    helper = case sec of
      SectionsEmpty -> return SectionsEmpty
      SectionsDefinitions d -> SectionsDefinitions <$> goDefinitions d
      SectionsNonDefinitions d -> SectionsNonDefinitions <$> goNonDefinitions d
      where
        goNonDefinitions :: NonDefinitionsSection 'Parsed -> Sem r' (NonDefinitionsSection 'Scoped)
        goNonDefinitions NonDefinitionsSection {..} = do
          sec' <- mapM goNonDefinition _nonDefinitionsSection
          next' <- mapM goDefinitions _nonDefinitionsNext
          return
            NonDefinitionsSection
              { _nonDefinitionsNext = next',
                _nonDefinitionsSection = sec'
              }
          where
            goNonDefinition :: NonDefinition 'Parsed -> Sem r' (NonDefinition 'Scoped)
            goNonDefinition = \case
              NonDefinitionModule m -> NonDefinitionModule <$> checkLocalModule m
              NonDefinitionImport i -> NonDefinitionImport <$> checkImport i
              NonDefinitionOpenModule i -> NonDefinitionOpenModule <$> checkOpenModule i

        goDefinitions :: DefinitionsSection 'Parsed -> Sem r' (DefinitionsSection 'Scoped)
        goDefinitions DefinitionsSection {..} = do
          mapM_ reserveDefinition _definitionsSection
          mapM_ scanAlias (_definitionsSection ^.. each . _DefinitionSyntax . _SyntaxAlias)
          sec' <- mapM goDefinition _definitionsSection
          next' <- mapM goNonDefinitions _definitionsNext
          return
            DefinitionsSection
              { _definitionsNext = next',
                _definitionsSection = sec'
              }
          where
            scanAlias :: AliasDef 'Parsed -> Sem r' ()
            scanAlias a = do
              aliasId <- gets (^?! scopeLocalSymbols . at (a ^. aliasDefName) . _Just . S.nameId)
              asName <- checkName (a ^. aliasDefAsName)
              modify' (set (scoperAlias . at aliasId) (Just asName))
              checkLoop aliasId
              where
                checkLoop :: NameId -> Sem r' ()
                checkLoop = evalState (mempty :: HashSet NameId) . go
                  where
                    go :: (Members '[State (HashSet NameId), Error ScoperError, State ScoperState] s) => NameId -> Sem s ()
                    go i = do
                      whenM (gets (HashSet.member i)) (throw (ErrAliasCycle (AliasCycle a)))
                      modify' (HashSet.insert i)
                      whenJustM (gets (^? scoperAlias . at i . _Just . preSymbolName . S.nameId)) go

            reserveDefinition :: Definition 'Parsed -> Sem r' ()
            reserveDefinition = \case
              DefinitionSyntax s -> resolveSyntaxDef s
              DefinitionFunctionDef d -> void (reserveFunctionSymbol d)
              DefinitionAxiom d -> void (reserveAxiomSymbol d)
              DefinitionProjectionDef d -> void (reserveProjectionSymbol d)
              DefinitionInductive d -> reserveInductive d
              where
                reserveInductive :: InductiveDef 'Parsed -> Sem r' ()
                reserveInductive d = do
                  i <- reserveInductiveSymbol d
                  constrs <- mapM reserveConstructor (d ^. inductiveConstructors)
                  void (defineInductiveModule (head constrs) d)
                  ignoreFail (registerRecordType (head constrs) i)
                  where
                    reserveConstructor :: ConstructorDef 'Parsed -> Sem r' S.Symbol
                    reserveConstructor c = do
                      c' <- reserveConstructorSymbol d c
                      let storeSig :: RecordNameSignature -> Sem r' ()
                          storeSig sig = modify' (set (scoperConstructorFields . at (c' ^. S.nameId)) (Just sig))
                      whenJust (c ^? constructorRhs . _ConstructorRhsRecord) (storeSig . mkRecordNameSignature)
                      return c'

                    registerRecordType :: S.Symbol -> S.Symbol -> Sem (Fail ': r') ()
                    registerRecordType mconstr ind = do
                      case d ^. inductiveConstructors of
                        mkRec :| cs
                          | not (null cs) -> fail
                          | otherwise -> do
                              fs <-
                                failMaybe $
                                  mkRec
                                    ^? constructorRhs
                                    . _ConstructorRhsRecord
                                    . to mkRecordNameSignature
                              let info =
                                    RecordInfo
                                      { _recordInfoSignature = fs,
                                        _recordInfoConstructor = mconstr
                                      }
                              modify' (set (scoperRecordFields . at (ind ^. S.nameId)) (Just info))

            goDefinition :: Definition 'Parsed -> Sem r' (Definition 'Scoped)
            goDefinition = \case
              DefinitionSyntax s -> DefinitionSyntax <$> checkSyntaxDef s
              DefinitionFunctionDef d -> DefinitionFunctionDef <$> checkFunctionDef d
              DefinitionAxiom d -> DefinitionAxiom <$> checkAxiomDef d
              DefinitionInductive d -> DefinitionInductive <$> checkInductiveDef d
              DefinitionProjectionDef d -> DefinitionProjectionDef <$> checkProjectionDef d

            defineInductiveModule :: S.Symbol -> InductiveDef 'Parsed -> Sem r' ()
            defineInductiveModule headConstr i = do
              m <- runReader (getLoc (i ^. inductiveName)) genModule
              checkLocalModule m >>= output
              where
                genModule :: forall s'. (Members '[Reader Interval] s') => Sem s' (Module 'Parsed 'ModuleLocal)
                genModule = do
                  _moduleKw <- G.kw G.kwModule
                  _moduleKwEnd <- G.kw G.kwEnd
                  let _modulePath = i ^. inductiveName
                  _moduleBody <- genBody
                  return
                    Module
                      { _moduleDoc = Nothing,
                        _modulePragmas = Nothing,
                        _moduleInductive = True,
                        ..
                      }
                  where
                    genBody :: Sem s' [Statement 'Parsed]
                    genBody = fromMaybe [] <$> runFail genFieldProjections
                      where
                        genFieldProjections :: Sem (Fail ': s') [Statement 'Parsed]
                        genFieldProjections = do
                          fs <- indexFrom 0 . toList <$> getFields
                          return (map (StatementProjectionDef . mkProjection) fs)
                          where
                            mkProjection ::
                              Indexed (RecordField 'Parsed) ->
                              ProjectionDef 'Parsed
                            mkProjection (Indexed idx field) =
                              ProjectionDef
                                { _projectionConstructor = headConstr,
                                  _projectionField = field ^. fieldName,
                                  _projectionFieldIx = idx
                                }

                            getFields :: Sem (Fail ': s') (NonEmpty (RecordField 'Parsed))
                            getFields = case i ^. inductiveConstructors of
                              c :| [] -> case c ^. constructorRhs of
                                ConstructorRhsRecord r -> return (r ^. rhsRecordFields)
                                _ -> fail
                              _ -> fail

mkLetSections :: [LetStatement 'Parsed] -> StatementSections 'Parsed
mkLetSections = mkSections . map toTopStatement
  where
    toTopStatement :: LetStatement 'Parsed -> Statement 'Parsed
    toTopStatement = \case
      LetFunctionDef f -> StatementFunctionDef f
      LetAliasDef f -> StatementSyntax (SyntaxAlias f)

mkSections :: [Statement 'Parsed] -> StatementSections 'Parsed
mkSections = \case
  [] -> SectionsEmpty
  h : hs -> case fromStatement h of
    Left d -> SectionsDefinitions (goDefinitions (pure d) hs)
    Right d -> SectionsNonDefinitions (goNonDefinitions (pure d) hs)
  where
    goDefinitions :: NonEmpty (Definition 'Parsed) -> [Statement 'Parsed] -> DefinitionsSection 'Parsed
    goDefinitions acc = \case
      s : ss -> case fromStatement s of
        Left d -> goDefinitions (NonEmpty.cons d acc) ss
        Right d ->
          DefinitionsSection
            { _definitionsSection = NonEmpty.reverse acc,
              _definitionsNext = Just (goNonDefinitions (pure d) ss)
            }
      [] ->
        DefinitionsSection
          { _definitionsSection = NonEmpty.reverse acc,
            _definitionsNext = Nothing
          }
    goNonDefinitions :: NonEmpty (NonDefinition 'Parsed) -> [Statement 'Parsed] -> NonDefinitionsSection 'Parsed
    goNonDefinitions acc = \case
      s : ss -> case fromStatement s of
        Right d -> goNonDefinitions (NonEmpty.cons d acc) ss
        Left d ->
          NonDefinitionsSection
            { _nonDefinitionsSection = NonEmpty.reverse acc,
              _nonDefinitionsNext = Just (goDefinitions (pure d) ss)
            }
      [] ->
        NonDefinitionsSection
          { _nonDefinitionsSection = NonEmpty.reverse acc,
            _nonDefinitionsNext = Nothing
          }
    fromStatement :: Statement 'Parsed -> Either (Definition 'Parsed) (NonDefinition 'Parsed)
    fromStatement = \case
      StatementAxiom a -> Left (DefinitionAxiom a)
      StatementFunctionDef n -> Left (DefinitionFunctionDef n)
      StatementInductive i -> Left (DefinitionInductive i)
      StatementSyntax s -> Left (DefinitionSyntax s)
      StatementProjectionDef s -> Left (DefinitionProjectionDef s)
      StatementImport i -> Right (NonDefinitionImport i)
      StatementModule m -> Right (NonDefinitionModule m)
      StatementOpenModule o -> Right (NonDefinitionOpenModule o)

reserveLocalModuleSymbol ::
  (Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r) =>
  Symbol ->
  Sem r S.Symbol
reserveLocalModuleSymbol =
  ignoreSyntax . reserveSymbolOf SKNameLocalModule Nothing

checkLocalModule ::
  forall r.
  (Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r) =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule Module {..} = do
  (_moduleExportInfo, moduleBody', moduleDoc') <-
    withLocalScope $ do
      inheritScope
      (e, b) <- checkModuleBody _moduleBody
      doc' <- mapM checkJudoc _moduleDoc
      return (e, b, doc')
  _modulePath' <- reserveLocalModuleSymbol _modulePath
  let moduleId = _modulePath' ^. S.nameId
      _moduleRefName = S.unConcrete _modulePath'
      _moduleRefModule =
        Module
          { _modulePath = _modulePath',
            _moduleBody = moduleBody',
            _moduleDoc = moduleDoc',
            _modulePragmas = _modulePragmas,
            _moduleKw,
            _moduleInductive,
            _moduleKwEnd
          }
      mref :: ModuleRef' 'S.NotConcrete
      mref = mkModuleRef' @'ModuleLocal ModuleRef'' {..}
  modify (over scoperModules (HashMap.insert moduleId mref))
  registerName _modulePath'
  return _moduleRefModule
  where
    inheritScope :: Sem r ()
    inheritScope = do
      absPath <- (S.<.> _modulePath) <$> gets (^. scopePath)
      modify (set scopePath absPath)
      modify (over scopeSymbols (fmap inheritSymbol))
      modify (over scopeModuleSymbols (fmap inheritSymbol))
      where
        inheritSymbol :: forall ns. (SingI ns) => SymbolInfo ns -> SymbolInfo ns
        inheritSymbol (SymbolInfo s) = SymbolInfo (inheritEntry <$> s)
          where
            inheritEntry :: NameSpaceEntryType ns -> NameSpaceEntryType ns
            inheritEntry =
              over (nsEntry . S.nameWhyInScope) S.BecauseInherited
                . set (nsEntry . S.nameVisibilityAnn) VisPrivate

checkOrphanOperators :: forall r. (Members '[Error ScoperError, State ScoperSyntax] r) => Sem r ()
checkOrphanOperators = do
  declared <- gets (^. scoperSyntaxOperators . scoperOperators)
  let unused = fmap (^. symbolOperatorDef) . find (^. symbolOperatorUsed . to not) . toList $ declared
  case unused of
    Nothing -> return ()
    Just x -> throw (ErrUnusedOperatorDef (UnusedOperatorDef x))

checkOrphanIterators :: forall r. (Members '[Error ScoperError, State ScoperSyntax] r) => Sem r ()
checkOrphanIterators = do
  declared <- gets (^. scoperSyntaxIterators . scoperIterators)
  let unused = fmap (^. symbolIteratorDef) . find (^. symbolIteratorUsed . to not) . toList $ declared
  case unused of
    Nothing -> return ()
    Just x -> throw (ErrUnusedIteratorDef (UnusedIteratorDef x))

symbolInfoSingle :: (SingI ns) => NameSpaceEntryType ns -> SymbolInfo ns
symbolInfoSingle p = SymbolInfo $ HashMap.singleton (p ^. nsEntry . S.nameDefinedIn) p

getModuleRef ::
  (Members '[State ScoperState] r) =>
  ModuleSymbolEntry ->
  Name ->
  Sem r ModuleRef
getModuleRef e n =
  overModuleRef'' (set (moduleRefName . S.nameConcrete) n)
    <$> gets (^?! scoperModules . at (e ^. moduleEntry . S.nameId) . _Just)

lookupModuleSymbol ::
  (Members '[Error ScoperError, State Scope, State ScoperState] r) =>
  Name ->
  Sem r ModuleRef
lookupModuleSymbol n = do
  es <- snd3 <$> lookupQualifiedSymbol (path, sym)
  case nonEmpty (resolveShadowing es) of
    Nothing -> notInScope
    Just (x :| []) -> getModuleRef x n
    Just more -> throw (ErrAmbiguousModuleSym (AmbiguousModuleSym n more))
  where
    notInScope = throw (ErrModuleNotInScope (ModuleNotInScope n))
    (path, sym) = case n of
      NameUnqualified s -> ([], s)
      NameQualified (QualifiedName (SymbolPath p) s) -> (toList p, s)

checkOpenImportModule ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenImportModule op
  | Just k <- op ^. openModuleImportKw =
      let import_ :: Import 'Parsed
          import_ =
            Import
              { _importKw = k,
                _importModule = moduleNameToTopModulePath (op ^. openModuleName),
                _importAsName = op ^. openImportAsName
              }
       in do
            import' <- checkImport import_
            let topName :: S.TopModulePath = over S.nameConcrete moduleNameToTopModulePath (import' ^. importModule . moduleRefName)
                op' =
                  op
                    { _openModuleImportKw = Nothing,
                      _openImportAsName = Nothing,
                      _openModuleName = maybe (op ^. openModuleName) topModulePathToName (op ^. openImportAsName)
                    }
            scopedOpen <- checkOpenModuleNoImport (Just (import' ^. importModule)) op'
            return
              scopedOpen
                { _openModuleImportKw = Just k,
                  _openModuleName = project (import' ^. importModule),
                  _openImportAsName = (\asTxt -> set S.nameConcrete asTxt topName) <$> op ^. openImportAsName
                }
  | otherwise = impossible

checkOpenModuleNoImport ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Maybe (ModuleRef'' 'S.Concrete 'ModuleTop) ->
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenModuleNoImport importModuleHint OpenModule {..}
  | isJust _openModuleImportKw = impossible
  | otherwise = do
      openModuleName'@(ModuleRef' (_ :&: moduleRef'')) <- case importModuleHint of
        Nothing -> lookupModuleSymbol _openModuleName
        Just m -> return (project m)
      let exportInfo = moduleRef'' ^. moduleExportInfo
      registerName (moduleRef'' ^. moduleRefName)

      let checkUsingHiding :: UsingHiding 'Parsed -> Sem r (UsingHiding 'Scoped)
          checkUsingHiding = \case
            Hiding h -> Hiding <$> checkHidingList h
            Using uh -> Using <$> checkUsingList uh
            where
              scopeSymbol :: forall (ns :: NameSpace). (SingI ns) => Sing ns -> Symbol -> Sem r S.Symbol
              scopeSymbol _ s = do
                let mentry :: Maybe (NameSpaceEntryType ns)
                    mentry = exportInfo ^. exportNameSpace . at s
                    err =
                      throw
                        ( ErrModuleDoesNotExportSymbol
                            ( ModuleDoesNotExportSymbol
                                { _moduleDoesNotExportSymbol = s,
                                  _moduleDoesNotExportModule = openModuleName'
                                }
                            )
                        )
                entry <- maybe err return mentry
                let scopedSym = entryToSymbol entry s
                registerName scopedSym
                return scopedSym

              checkHidingList :: HidingList 'Parsed -> Sem r (HidingList 'Scoped)
              checkHidingList l = do
                items' <- mapM checkHidingItem (l ^. hidingList)
                return
                  HidingList
                    { _hidingKw = l ^. hidingKw,
                      _hidingBraces = l ^. hidingBraces,
                      _hidingList = items'
                    }

              checkUsingList :: UsingList 'Parsed -> Sem r (UsingList 'Scoped)
              checkUsingList l = do
                items' <- mapM checkUsingItem (l ^. usingList)
                return
                  UsingList
                    { _usingKw = l ^. usingKw,
                      _usingBraces = l ^. usingBraces,
                      _usingList = items'
                    }

              checkHidingItem :: HidingItem 'Parsed -> Sem r (HidingItem 'Scoped)
              checkHidingItem h = do
                let s = h ^. hidingSymbol
                scopedSym <-
                  if
                      | isJust (h ^. hidingModuleKw) -> scopeSymbol SNameSpaceModules s
                      | otherwise -> scopeSymbol SNameSpaceSymbols s
                return
                  HidingItem
                    { _hidingSymbol = scopedSym,
                      _hidingModuleKw = h ^. hidingModuleKw
                    }

              checkUsingItem :: UsingItem 'Parsed -> Sem r (UsingItem 'Scoped)
              checkUsingItem i = do
                let s = i ^. usingSymbol
                scopedSym <-
                  if
                      | isJust (i ^. usingModuleKw) -> scopeSymbol SNameSpaceModules s
                      | otherwise -> scopeSymbol SNameSpaceSymbols s
                let scopedAs = do
                      c <- i ^. usingAs
                      return (set S.nameConcrete c scopedSym)
                mapM_ registerName scopedAs
                return
                  UsingItem
                    { _usingSymbol = scopedSym,
                      _usingAs = scopedAs,
                      _usingAsKw = i ^. usingAsKw,
                      _usingModuleKw = i ^. usingModuleKw
                    }

      usingHiding' <- mapM checkUsingHiding _openUsingHiding
      mergeScope (alterScope usingHiding' exportInfo)
      return
        OpenModule
          { _openModuleName = openModuleName',
            _openImportAsName = Nothing,
            _openUsingHiding = usingHiding',
            ..
          }
  where
    mergeScope :: ExportInfo -> Sem r ()
    mergeScope ei = do
      mapM_ mergeSymbol (HashMap.toList (ei ^. exportSymbols))
      mapM_ mergeSymbol (HashMap.toList (ei ^. exportModuleSymbols))
      mapM_ mergeSymbol (HashMap.toList (ei ^. exportFixitySymbols))
      where
        mergeSymbol :: forall ns. (SingI ns) => (Symbol, NameSpaceEntryType ns) -> Sem r ()
        mergeSymbol (s, entry) =
          modify
            (over scopeNameSpace (HashMap.insertWith (<>) s (symbolInfoSingle entry)))

    alterScope :: Maybe (UsingHiding 'Scoped) -> ExportInfo -> ExportInfo
    alterScope openModif = alterEntries . filterScope
      where
        alterEntries :: ExportInfo -> ExportInfo
        alterEntries nfo =
          ExportInfo
            { _exportSymbols = alterEntry <$> nfo ^. exportSymbols,
              _exportModuleSymbols = alterEntry <$> nfo ^. exportModuleSymbols,
              _exportFixitySymbols = alterEntry <$> nfo ^. exportFixitySymbols
            }

        alterEntry :: (SingI ns) => NameSpaceEntryType ns -> NameSpaceEntryType ns
        alterEntry =
          over
            nsEntry
            ( set S.nameWhyInScope S.BecauseImportedOpened
                . set S.nameVisibilityAnn (publicAnnToVis _openPublic)
            )

        publicAnnToVis :: PublicAnn -> VisibilityAnn
        publicAnnToVis = \case
          Public -> VisPublic
          NoPublic -> VisPrivate

        filterScope :: ExportInfo -> ExportInfo
        filterScope = case openModif of
          Just (Using l) ->
            over exportSymbols (HashMap.fromList . mapMaybe inUsing . HashMap.toList)
              . over exportModuleSymbols (HashMap.fromList . mapMaybe inUsing . HashMap.toList)
              . over exportFixitySymbols (HashMap.fromList . mapMaybe inUsing . HashMap.toList)
            where
              inUsing ::
                forall (ns :: NameSpace).
                (SingI ns) =>
                (Symbol, NameSpaceEntryType ns) ->
                Maybe (Symbol, NameSpaceEntryType ns)
              inUsing (sym, e) = do
                mayAs' <- u ^. at (e ^. nsEntry . S.nameId)
                return (fromMaybe sym mayAs', e)
              u :: HashMap NameId (Maybe Symbol)
              u =
                HashMap.fromList
                  [ (i ^. usingSymbol . S.nameId, i ^? usingAs . _Just . S.nameConcrete)
                    | i <- toList (l ^. usingList)
                  ]
          Just (Hiding l) ->
            over exportSymbols (HashMap.filter (not . inHiding))
              . over exportModuleSymbols (HashMap.filter (not . inHiding))
              . over exportFixitySymbols (HashMap.filter (not . inHiding))
            where
              inHiding :: forall ns. (SingI ns) => NameSpaceEntryType ns -> Bool
              inHiding e = HashSet.member (e ^. nsEntry . S.nameId) u
              u :: HashSet NameId
              u = HashSet.fromList (map (^. hidingSymbol . S.nameId) (toList (l ^. hidingList)))
          Nothing -> id

checkOpenModule ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenModule op
  | isJust (op ^. openModuleImportKw) = checkOpenImportModule op
  | otherwise = checkOpenModuleNoImport Nothing op

checkAxiomDef ::
  (Members '[Reader ScopeParameters, InfoTableBuilder, Error ScoperError, State Scope, State ScoperState, NameIdGen, State ScoperSyntax, Reader BindingStrategy] r) =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiomDef AxiomDef {..} = do
  axiomType' <- withLocalScope (checkParseExpressionAtoms _axiomType)
  axiomName' <- bindAxiomSymbol _axiomName
  axiomDoc' <- withLocalScope (mapM checkJudoc _axiomDoc)
  registerAxiom @$> AxiomDef {_axiomName = axiomName', _axiomType = axiomType', _axiomDoc = axiomDoc', ..}

entryToSymbol :: forall (ns :: NameSpace). (SingI ns) => NameSpaceEntryType ns -> Symbol -> S.Symbol
entryToSymbol sentry csym = set S.nameConcrete csym (sentry ^. nsEntry)

checkFunction ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction f = do
  _paramType <- checkParseExpressionAtoms (f ^. funParameters . paramType)
  withLocalScope $ do
    _paramNames <- forM (f ^. funParameters . paramNames) $ \case
      FunctionParameterWildcard w -> return (FunctionParameterWildcard w)
      FunctionParameterName p -> FunctionParameterName <$> bindVariableSymbol p
    _funReturn <- checkParseExpressionAtoms (f ^. funReturn)
    let _paramImplicit = f ^. funParameters . paramImplicit
        _paramColon = f ^. funParameters . paramColon
        _paramDelims = f ^. funParameters . paramDelims
        _funParameters = FunctionParameters {..}
        _funKw = f ^. funKw
    return Function {..}

-- | for now functions defined in let clauses cannot be infix operators
checkLetFunDefs ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  NonEmpty (LetStatement 'Parsed) ->
  Sem r (NonEmpty (LetStatement 'Scoped))
checkLetFunDefs =
  localBindings
    . ignoreSyntax
    . fmap fromSections
    . checkSections
    . mkLetSections
    . toList
  where
    fromSections :: StatementSections s -> NonEmpty (LetStatement s)
    fromSections = \case
      SectionsEmpty -> impossible
      SectionsDefinitions d -> fromDefs d
      SectionsNonDefinitions d -> fromNonDefs d
      where
        fromDefs :: DefinitionsSection s -> NonEmpty (LetStatement s)
        fromDefs DefinitionsSection {..} =
          (fromDef <$> _definitionsSection) <>? (fromNonDefs <$> _definitionsNext)
          where
            fromSyn :: SyntaxDef s -> LetStatement s
            fromSyn = \case
              SyntaxAlias a -> LetAliasDef a
              SyntaxFixity {} -> impossible
              SyntaxOperator {} -> impossible
              SyntaxIterator {} -> impossible

            fromDef :: Definition s -> LetStatement s
            fromDef = \case
              DefinitionFunctionDef d -> LetFunctionDef d
              DefinitionSyntax syn -> fromSyn syn
              DefinitionInductive {} -> impossible
              DefinitionProjectionDef {} -> impossible
              DefinitionAxiom {} -> impossible
        fromNonDefs :: NonDefinitionsSection s -> NonEmpty (LetStatement s)
        fromNonDefs NonDefinitionsSection {..} =
          (fromNonDef <$> _nonDefinitionsSection) <>? (fromDefs <$> _nonDefinitionsNext)
          where
            fromNonDef :: NonDefinition s -> LetStatement s
            fromNonDef = \case
              NonDefinitionImport {} -> impossible
              NonDefinitionModule {} -> impossible
              NonDefinitionOpenModule {} -> impossible

checkRecordPattern ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  RecordPattern 'Parsed ->
  Sem r (RecordPattern 'Scoped)
checkRecordPattern r = do
  c' <- getNameOfKind KNameConstructor (r ^. recordPatternConstructor)
  fields <- fromMaybeM (return (RecordNameSignature mempty)) (gets (^. scoperConstructorFields . at (c' ^. scopedIdenName . S.nameId)))
  l' <-
    if
        | null (r ^. recordPatternItems) -> return []
        | otherwise -> do
            when (null (fields ^. recordNames)) (throw (noFields c'))
            runReader fields (mapM checkItem (r ^. recordPatternItems))
  return
    RecordPattern
      { _recordPatternConstructor = c',
        _recordPatternSignature = Irrelevant fields,
        _recordPatternItems = l'
      }
  where
    noFields :: ScopedIden -> ScoperError
    noFields = ErrConstructorNotARecord . ConstructorNotARecord
    checkItem ::
      forall r'.
      (Members '[Reader RecordNameSignature, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r') =>
      RecordPatternItem 'Parsed ->
      Sem r' (RecordPatternItem 'Scoped)
    checkItem = \case
      RecordPatternItemAssign a -> RecordPatternItemAssign <$> checkAssign a
      RecordPatternItemFieldPun a -> RecordPatternItemFieldPun <$> checkPun a
      where
        findField :: Symbol -> Sem r' Int
        findField f = fromMaybeM (throw err) (asks (^? recordNames . at f . _Just . _2))
          where
            err :: ScoperError
            err = ErrUnexpectedField (UnexpectedField f)

        checkAssign :: RecordPatternAssign 'Parsed -> Sem r' (RecordPatternAssign 'Scoped)
        checkAssign RecordPatternAssign {..} = do
          idx' <- findField _recordPatternAssignField
          pat' <- checkParsePatternAtoms _recordPatternAssignPattern
          return
            RecordPatternAssign
              { _recordPatternAssignFieldIx = idx',
                _recordPatternAssignPattern = pat',
                ..
              }

        checkPun :: FieldPun 'Parsed -> Sem r' (FieldPun 'Scoped)
        checkPun f = do
          idx' <- findField (f ^. fieldPunField)
          f' <- bindVariableSymbol (f ^. fieldPunField)
          return
            FieldPun
              { _fieldPunIx = idx',
                _fieldPunField = f'
              }

checkListPattern ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ListPattern 'Parsed ->
  Sem r (ListPattern 'Scoped)
checkListPattern l = do
  let _listpBracketL = l ^. listpBracketL
      _listpBracketR = l ^. listpBracketR
  _listpItems <- mapM checkParsePatternAtoms (l ^. listpItems)
  return ListPattern {..}

checkList ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  List 'Parsed ->
  Sem r (List 'Scoped)
checkList l = do
  let _listBracketL = l ^. listBracketL
      _listBracketR = l ^. listBracketR
  _listItems <- mapM checkParseExpressionAtoms (l ^. listItems)
  return List {..}

checkLet ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Let 'Parsed ->
  Sem r (Let 'Scoped)
checkLet Let {..} =
  withLocalScope $ do
    letFunDefs' <- checkLetFunDefs _letFunDefs
    letExpression' <- checkParseExpressionAtoms _letExpression
    return
      Let
        { _letFunDefs = letFunDefs',
          _letExpression = letExpression',
          _letKw,
          _letInKw
        }

checkCaseBranch ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  CaseBranch 'Parsed ->
  Sem r (CaseBranch 'Scoped)
checkCaseBranch CaseBranch {..} = withLocalScope $ do
  pattern' <- checkParsePatternAtoms _caseBranchPattern
  expression' <- (checkParseExpressionAtoms _caseBranchExpression)
  return $
    CaseBranch
      { _caseBranchPattern = pattern',
        _caseBranchExpression = expression',
        ..
      }

checkNewCaseBranch ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  NewCaseBranch 'Parsed ->
  Sem r (NewCaseBranch 'Scoped)
checkNewCaseBranch NewCaseBranch {..} = withLocalScope $ do
  pattern' <- checkParsePatternAtoms _newCaseBranchPattern
  expression' <- (checkParseExpressionAtoms _newCaseBranchExpression)
  return $
    NewCaseBranch
      { _newCaseBranchPattern = pattern',
        _newCaseBranchExpression = expression',
        ..
      }

checkCase ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Case 'Parsed ->
  Sem r (Case 'Scoped)
checkCase Case {..} = do
  caseBranches' <- mapM checkCaseBranch _caseBranches
  caseExpression' <- checkParseExpressionAtoms _caseExpression
  return $
    Case
      { _caseExpression = caseExpression',
        _caseBranches = caseBranches',
        _caseKw,
        _caseParens
      }

checkNewCase ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  NewCase 'Parsed ->
  Sem r (NewCase 'Scoped)
checkNewCase NewCase {..} = do
  caseBranches' <- mapM checkNewCaseBranch _newCaseBranches
  caseExpression' <- checkParseExpressionAtoms _newCaseExpression
  return $
    NewCase
      { _newCaseExpression = caseExpression',
        _newCaseBranches = caseBranches',
        _newCaseKw,
        _newCaseOfKw
      }

checkLambda ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda Lambda {..} = do
  clauses' <- mapM checkLambdaClause _lambdaClauses
  return
    Lambda
      { _lambdaClauses = clauses',
        _lambdaKw,
        _lambdaBraces
      }

checkLambdaClause ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  LambdaClause 'Parsed ->
  Sem r (LambdaClause 'Scoped)
checkLambdaClause LambdaClause {..} = withLocalScope $ do
  lambdaParameters' <- mapM checkParsePatternAtom _lambdaParameters
  lambdaBody' <- checkParseExpressionAtoms _lambdaBody
  return
    LambdaClause
      { _lambdaParameters = lambdaParameters',
        _lambdaBody = lambdaBody',
        _lambdaPipe,
        _lambdaAssignKw
      }

checkUnqualifiedName ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  Symbol ->
  Sem r PreSymbolEntry
checkUnqualifiedName s = do
  scope <- get
  -- Lookup at the global scope
  entries <- fst3 <$> lookupQualifiedSymbol ([], s)
  case resolveShadowing entries of
    [] -> throw (ErrSymNotInScope (NotInScope s scope))
    [x] -> return x
    es -> throw (ErrAmbiguousSym (AmbiguousSym n es))
  where
    n = NameUnqualified s

checkFixitySymbol ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  Symbol ->
  Sem r S.Symbol
checkFixitySymbol s = do
  scope <- get
  -- Lookup at the global scope
  entries <- thd3 <$> lookupQualifiedSymbol ([], s)
  case resolveShadowing entries of
    [] -> throw (ErrSymNotInScope (NotInScope s scope))
    [x] -> do
      let res = entryToSymbol x s
      registerName res
      return res
    es -> throw (ErrAmbiguousSym (AmbiguousSym n (map (PreSymbolFinal . SymbolEntry . (^. fixityEntry)) es)))
  where
    n = NameUnqualified s

-- | Remove the symbol entries associated with a single symbol according to the
-- shadowing rules for modules. For example, a symbol defined in the outer
-- module with the same name as a symbol defined in the inner module will be
-- removed.
resolveShadowing :: forall ns. (SingI ns) => [NameSpaceEntryType ns] -> [NameSpaceEntryType ns]
resolveShadowing es = go [(e, e ^. nsEntry . S.nameWhyInScope) | e <- es]
  where
    go :: [(NameSpaceEntryType ns, S.WhyInScope)] -> [NameSpaceEntryType ns]
    go itms
      | any (((== S.BecauseImportedOpened) .||. (== S.BecauseDefined)) . snd) itms =
          [e | (e, w) <- itms, not (isInherited w)]
      | (notNull .&&. all (isInherited . snd)) itms = go [(e, peelInherited w) | (e, w) <- itms]
      | otherwise = map fst itms
      where
        peelInherited :: S.WhyInScope -> S.WhyInScope
        peelInherited = \case
          S.BecauseInherited w -> w
          _ -> impossible

        isInherited :: S.WhyInScope -> Bool
        isInherited = \case
          S.BecauseInherited {} -> True
          _ -> False

checkPatternName ::
  forall r.
  (Members '[Error ScoperError, State Scope, NameIdGen, State ScoperState, InfoTableBuilder] r) =>
  Name ->
  Sem r PatternScopedIden
checkPatternName n = do
  c <- getConstructorRef
  case c of
    Just constr -> return (PatternScopedConstructor constr) -- the symbol is a constructor
    Nothing -> case n of
      NameUnqualified {} -> PatternScopedVar <$> bindVariableSymbol sym -- the symbol is a variable
      NameQualified {} -> nameNotInScope n
  where
    sym = snd (splitName n)
    getConstructorRef :: Sem r (Maybe ScopedIden)
    getConstructorRef = lookupNameOfKind KNameConstructor n

nameNotInScope :: forall r a. (Members '[Error ScoperError, State Scope] r) => Name -> Sem r a
nameNotInScope n = err >>= throw
  where
    err :: Sem r ScoperError
    err = case n of
      NameQualified q -> return (ErrQualSymNotInScope (QualSymNotInScope q))
      NameUnqualified s -> ErrSymNotInScope . NotInScope s <$> get

getNameOfKind ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  NameKind ->
  Name ->
  Sem r ScopedIden
getNameOfKind nameKind n = fromMaybeM (nameNotInScope n) (lookupNameOfKind nameKind n)

lookupNameOfKind ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  NameKind ->
  Name ->
  Sem r (Maybe ScopedIden)
lookupNameOfKind nameKind n = do
  entries <- lookupQualifiedSymbol (path, sym) >>= mapMaybeM filterEntry . fst3
  case entries of
    [] -> return Nothing
    [(_, s)] -> return (Just s) -- There is one constructor with such a name
    es -> throw (ErrAmbiguousSym (AmbiguousSym n (map fst es)))
  where
    (path, sym) = splitName n
    filterEntry :: PreSymbolEntry -> Sem r (Maybe (PreSymbolEntry, ScopedIden))
    filterEntry e = do
      e' <- entryToScopedIden n e
      return $ do
        guard (nameKind == getNameKind e')
        return (e, e')

checkPatternBinding ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternBinding ->
  Sem r PatternArg
checkPatternBinding (PatternBinding n p) = do
  p' <- checkParsePatternAtom p
  n' <- bindVariableSymbol n
  if
      | isJust (p' ^. patternArgName) -> throw (ErrDoubleBinderPattern (DoubleBinderPattern n' p'))
      | otherwise -> return (set patternArgName (Just n') p')

checkPatternAtoms ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternAtoms 'Parsed ->
  Sem r (PatternAtoms 'Scoped)
checkPatternAtoms (PatternAtoms s i) = (`PatternAtoms` i) <$> mapM checkPatternAtom s

checkParsePatternAtoms ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternAtoms 'Parsed ->
  Sem r PatternArg
checkParsePatternAtoms = checkPatternAtoms >=> parsePatternAtoms

checkPatternAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternAtom 'Parsed ->
  Sem r (PatternAtom 'Scoped)
checkPatternAtom = \case
  PatternAtomIden n -> PatternAtomIden <$> checkPatternName n
  PatternAtomWildcard i -> return (PatternAtomWildcard i)
  PatternAtomEmpty i -> return (PatternAtomEmpty i)
  PatternAtomParens e -> PatternAtomParens <$> checkParsePatternAtoms e
  PatternAtomBraces e -> PatternAtomBraces <$> checkParsePatternAtoms e
  PatternAtomDoubleBraces e -> PatternAtomDoubleBraces <$> checkParsePatternAtoms e
  PatternAtomAt p -> PatternAtomAt <$> checkPatternBinding p
  PatternAtomList l -> PatternAtomList <$> checkListPattern l
  PatternAtomRecord l -> PatternAtomRecord <$> checkRecordPattern l

checkName ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  Name ->
  Sem r PreSymbolEntry
checkName n = case n of
  NameQualified q -> checkQualifiedName q
  NameUnqualified s -> checkUnqualifiedName s

checkScopedIden ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  Name ->
  Sem r ScopedIden
checkScopedIden n = checkName n >>= entryToScopedIden n

checkExpressionAtom ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtom 'Parsed ->
  Sem r (NonEmpty (ExpressionAtom 'Scoped))
checkExpressionAtom e = case e of
  AtomIdentifier n -> pure . AtomIdentifier <$> checkScopedIden n
  AtomLambda lam -> pure . AtomLambda <$> checkLambda lam
  AtomCase c -> pure . AtomCase <$> checkCase c
  AtomNewCase c -> pure . AtomNewCase <$> checkNewCase c
  AtomLet letBlock -> pure . AtomLet <$> checkLet letBlock
  AtomUniverse uni -> return (pure (AtomUniverse uni))
  AtomFunction fun -> pure . AtomFunction <$> checkFunction fun
  AtomParens par -> pure . AtomParens <$> checkParens par
  AtomDoubleBraces br -> pure . AtomDoubleBraces <$> traverseOf doubleBracesExpression checkParseExpressionAtoms br
  AtomBraces br -> pure . AtomBraces <$> traverseOf withLocParam checkParseExpressionAtoms br
  AtomFunArrow a -> return (pure (AtomFunArrow a))
  AtomHole h -> pure . AtomHole <$> checkHole h
  AtomInstanceHole h -> pure . AtomInstanceHole <$> checkHole h
  AtomLiteral l -> return (pure (AtomLiteral l))
  AtomList l -> pure . AtomList <$> checkList l
  AtomIterator i -> pure . AtomIterator <$> checkIterator i
  AtomNamedApplication i -> pure . AtomNamedApplication <$> checkNamedApplication i
  AtomRecordUpdate i -> pure . AtomRecordUpdate <$> checkRecordUpdate i

checkRecordUpdate :: forall r. (Members '[Error ScoperError, State Scope, State ScoperState, Reader ScopeParameters, InfoTableBuilder, NameIdGen] r) => RecordUpdate 'Parsed -> Sem r (RecordUpdate 'Scoped)
checkRecordUpdate RecordUpdate {..} = do
  tyName' <- getNameOfKind KNameInductive _recordUpdateTypeName
  info <- getRecordInfo tyName'
  let sig = info ^. recordInfoSignature
  (vars', fields') <- withLocalScope $ do
    vs <- mapM bindVariableSymbol (toList (recordNameSignatureByIndex sig))
    fs <- mapM (checkField sig) _recordUpdateFields
    return (vs, fs)
  let extra' =
        RecordUpdateExtra
          { _recordUpdateExtraSignature = sig,
            _recordUpdateExtraVars = vars',
            _recordUpdateExtraConstructor = info ^. recordInfoConstructor
          }
  return
    RecordUpdate
      { _recordUpdateTypeName = tyName',
        _recordUpdateFields = fields',
        _recordUpdateExtra = Irrelevant extra',
        _recordUpdateAtKw,
        _recordUpdateDelims
      }
  where
    checkField :: RecordNameSignature -> RecordUpdateField 'Parsed -> Sem r (RecordUpdateField 'Scoped)
    checkField sig f = do
      value' <- checkParseExpressionAtoms (f ^. fieldUpdateValue)
      idx' <- maybe (throw unexpectedField) return (sig ^? recordNames . at (f ^. fieldUpdateName) . _Just . _2)
      return
        RecordUpdateField
          { _fieldUpdateName = f ^. fieldUpdateName,
            _fieldUpdateArgIx = idx',
            _fieldUpdateAssignKw = f ^. fieldUpdateAssignKw,
            _fieldUpdateValue = value'
          }
      where
        unexpectedField :: ScoperError
        unexpectedField = ErrUnexpectedField (UnexpectedField (f ^. fieldUpdateName))

checkNamedApplication ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  NamedApplication 'Parsed ->
  Sem r (NamedApplication 'Scoped)
checkNamedApplication napp = do
  _namedAppName <- checkScopedIden (napp ^. namedAppName)
  _namedAppSignature <- Irrelevant <$> getNameSignature _namedAppName
  _namedAppArgs <- mapM checkArgumentBlock (napp ^. namedAppArgs)
  return NamedApplication {..}
  where
    checkNamedArg :: NamedArgument 'Parsed -> Sem r (NamedArgument 'Scoped)
    checkNamedArg n = do
      let _namedArgName = n ^. namedArgName
          _namedArgAssignKw = n ^. namedArgAssignKw
      _namedArgValue <- checkParseExpressionAtoms (n ^. namedArgValue)
      return NamedArgument {..}
    checkArgumentBlock :: ArgumentBlock 'Parsed -> Sem r (ArgumentBlock 'Scoped)
    checkArgumentBlock b = do
      let _argBlockDelims = b ^. argBlockDelims
          _argBlockImplicit = b ^. argBlockImplicit
      _argBlockArgs <- mapM checkNamedArg (b ^. argBlockArgs)
      return ArgumentBlock {..}

getRecordInfo ::
  forall r.
  (Members '[State ScoperState, Error ScoperError] r) =>
  ScopedIden ->
  Sem r RecordInfo
getRecordInfo indTy =
  fromMaybeM err (gets (^. scoperRecordFields . at (indTy ^. scopedIdenFinal . S.nameId)))
  where
    err :: Sem r a
    err = throw (ErrNotARecord (NotARecord indTy))

getNameSignature :: (Members '[State ScoperState, Error ScoperError] r) => ScopedIden -> Sem r NameSignature
getNameSignature s = do
  sig <- maybeM (throw err) return (lookupNameSignature (s ^. scopedIdenFinal . S.nameId))
  when (null (sig ^. nameSignatureArgs)) (throw err)
  return sig
  where
    err = ErrNoNameSignature (NoNameSignature s)

lookupNameSignature :: (Members '[State ScoperState] r) => S.NameId -> Sem r (Maybe NameSignature)
lookupNameSignature s = gets (^. scoperSignatures . at s)

checkIterator ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Iterator 'Parsed ->
  Sem r (Iterator 'Scoped)
checkIterator iter = do
  _iteratorName <- checkScopedIden (iter ^. iteratorName)
  case _iteratorName ^. scopedIdenName . S.nameIterator of
    Just IteratorInfo {..} -> do
      case _iteratorInfoInitNum of
        Just n
          | n /= length (iter ^. iteratorInitializers) ->
              throw
                ( ErrIteratorInitializer
                    IteratorInitializer {_iteratorInitializerIterator = iter}
                )
        _ -> return ()
      case _iteratorInfoRangeNum of
        Just n
          | n /= length (iter ^. iteratorRanges) ->
              throw
                ( ErrIteratorRange
                    IteratorRange {_iteratorRangeIterator = iter}
                )
        _ -> return ()
    Nothing ->
      throw
        ( ErrIteratorUndefined
            IteratorUndefined {_iteratorUndefinedIterator = iter}
        )
  inivals' <- mapM (checkParseExpressionAtoms . (^. initializerExpression)) (iter ^. iteratorInitializers)
  rngvals' <- mapM (checkParseExpressionAtoms . (^. rangeExpression)) (iter ^. iteratorRanges)
  let initAssignKws = iter ^.. iteratorInitializers . each . initializerAssignKw
      rangesInKws = iter ^.. iteratorRanges . each . rangeInKw
  withLocalScope $ do
    inipats' <- mapM (checkParsePatternAtoms . (^. initializerPattern)) (iter ^. iteratorInitializers)
    rngpats' <- mapM (checkParsePatternAtoms . (^. rangePattern)) (iter ^. iteratorRanges)
    let _iteratorInitializers = [Initializer p k v | ((p, k), v) <- zipExact (zipExact inipats' initAssignKws) inivals']
        _iteratorRanges = [Range p k v | ((p, k), v) <- zipExact (zipExact rngpats' rangesInKws) rngvals']
        _iteratorParens = iter ^. iteratorParens
        _iteratorBodyBraces = iter ^. iteratorBodyBraces
    _iteratorBody <- checkParseExpressionAtoms (iter ^. iteratorBody)
    return Iterator {..}

checkInitializer ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Initializer 'Parsed ->
  Sem r (Initializer 'Scoped)
checkInitializer ini = do
  _initializerPattern <- checkParsePatternAtoms (ini ^. initializerPattern)
  _initializerExpression <- checkParseExpressionAtoms (ini ^. initializerExpression)
  return
    Initializer
      { _initializerAssignKw = ini ^. initializerAssignKw,
        ..
      }

checkRange ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Range 'Parsed ->
  Sem r (Range 'Scoped)
checkRange rng = do
  _rangePattern <- checkParsePatternAtoms (rng ^. rangePattern)
  _rangeExpression <- checkParseExpressionAtoms (rng ^. rangeExpression)
  return
    Range
      { _rangeInKw = rng ^. rangeInKw,
        ..
      }

checkHole ::
  (Members '[NameIdGen] r) =>
  HoleType 'Parsed ->
  Sem r Hole
checkHole h = do
  i <- freshNameId
  return
    Hole
      { _holeId = i,
        _holeKw = h
      }

checkParens ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParens e@(ExpressionAtoms as _) = case as of
  p :| [] -> case p of
    AtomIdentifier s -> do
      scopedId <- checkScopedIden s
      let scopedIdenNoFix = over scopedIdenName (set S.nameFixity Nothing) scopedId
      return (ExpressionParensIdentifier scopedIdenNoFix)
    AtomIterator i -> ExpressionIterator . set iteratorParens True <$> checkIterator i
    AtomCase c -> ExpressionCase . set caseParens True <$> checkCase c
    AtomRecordUpdate u -> ExpressionParensRecordUpdate . ParensRecordUpdate <$> checkRecordUpdate u
    _ -> checkParseExpressionAtoms e
  _ -> checkParseExpressionAtoms e

checkExpressionAtoms ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l i) = (`ExpressionAtoms` i) <$> sconcatMap checkExpressionAtom l

checkJudoc ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Judoc 'Parsed ->
  Sem r (Judoc 'Scoped)
checkJudoc (Judoc groups) = Judoc <$> mapM checkJudocGroup groups

checkJudocGroup ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocGroup 'Parsed ->
  Sem r (JudocGroup 'Scoped)
checkJudocGroup = \case
  JudocGroupBlock b -> JudocGroupBlock <$> checkJudocBlockParagraph b
  JudocGroupLines l -> JudocGroupLines <$> mapM checkJudocBlock l

checkJudocBlock ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocBlock 'Parsed ->
  Sem r (JudocBlock 'Scoped)
checkJudocBlock = \case
  JudocLines l -> JudocLines <$> mapM checkJudocLine l
  JudocExample e -> JudocExample <$> traverseOf exampleExpression checkParseExpressionAtoms e

checkJudocBlockParagraph ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocBlockParagraph 'Parsed ->
  Sem r (JudocBlockParagraph 'Scoped)
checkJudocBlockParagraph = traverseOf judocBlockParagraphBlocks (mapM checkJudocBlock)

checkJudocLine ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocLine 'Parsed ->
  Sem r (JudocLine 'Scoped)
checkJudocLine (JudocLine delim atoms) = JudocLine delim <$> mapM (mapM checkJudocAtom) atoms

checkJudocAtom ::
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocAtom 'Parsed ->
  Sem r (JudocAtom 'Scoped)
checkJudocAtom = \case
  JudocText t -> return (JudocText t)
  JudocExpression e -> JudocExpression <$> checkParseExpressionAtoms e

checkParseExpressionAtoms ::
  forall r.
  (Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternAtom 'Parsed ->
  Sem r PatternArg
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkSyntaxDef ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperSyntax] r) =>
  SyntaxDef 'Parsed ->
  Sem r (SyntaxDef 'Scoped)
checkSyntaxDef = \case
  SyntaxFixity fixDef -> SyntaxFixity <$> checkFixitySyntaxDef fixDef
  SyntaxAlias a -> SyntaxAlias <$> checkAliasDef a
  SyntaxOperator opDef -> return $ SyntaxOperator opDef
  SyntaxIterator iterDef -> return $ SyntaxIterator iterDef

checkAliasDef ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperSyntax] r) =>
  AliasDef 'Parsed ->
  Sem r (AliasDef 'Scoped)
checkAliasDef AliasDef {..} = do
  aliasName' :: S.Symbol <- gets (^?! scopeLocalSymbols . at _aliasDefName . _Just)
  asName' <- checkScopedIden _aliasDefAsName
  return
    AliasDef
      { _aliasDefName = aliasName',
        _aliasDefAsName = asName',
        ..
      }

reserveAliasDef ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperSyntax, Reader BindingStrategy] r) =>
  AliasDef 'Parsed ->
  Sem r ()
reserveAliasDef = void . reserveAliasSymbol . (^. aliasDefName)

resolveSyntaxDef ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperSyntax, Reader BindingStrategy] r) =>
  SyntaxDef 'Parsed ->
  Sem r ()
resolveSyntaxDef = \case
  SyntaxFixity fixDef -> resolveFixitySyntaxDef fixDef
  SyntaxOperator opDef -> resolveOperatorSyntaxDef opDef
  SyntaxIterator iterDef -> resolveIteratorSyntaxDef iterDef
  SyntaxAlias a -> reserveAliasDef a

-------------------------------------------------------------------------------
-- Check precedences are comparable
-------------------------------------------------------------------------------

checkPrecedences ::
  forall r.
  (Members '[Error ScoperError, InfoTableBuilder] r) =>
  [S.Name] ->
  Sem r ()
checkPrecedences opers = do
  tab <- getInfoTable
  let fids = mapMaybe (^. fixityId) $ mapMaybe (^. S.nameFixity) opers
      deps = createDependencyInfo (tab ^. infoPrecedenceGraph) mempty
  mapM_ (uncurry (checkPath deps)) $
    [(fid1, fid2) | fid1 <- fids, fid2 <- fids, fid1 /= fid2]
  where
    checkPath :: DependencyInfo S.NameId -> S.NameId -> S.NameId -> Sem r ()
    checkPath deps fid1 fid2 =
      unless (isPath deps fid1 fid2 || isPath deps fid2 fid1) $
        throw (ErrIncomparablePrecedences (IncomaprablePrecedences (findOper fid1) (findOper fid2)))

    findOper :: S.NameId -> S.Name
    findOper fid =
      fromJust $
        find
          (maybe False (\fx -> Just fid == (fx ^. fixityId)) . (^. S.nameFixity))
          opers

checkExpressionPrecedences :: (Members '[Error ScoperError, InfoTableBuilder] r) => ExpressionAtoms 'Scoped -> Sem r ()
checkExpressionPrecedences (ExpressionAtoms atoms _) =
  checkPrecedences opers
  where
    opers :: [S.Name]
    opers = mapMaybe P.getExpressionAtomIden (toList atoms)

checkPatternPrecedences :: (Members '[Error ScoperError, InfoTableBuilder] r) => PatternAtoms 'Scoped -> Sem r ()
checkPatternPrecedences (PatternAtoms atoms _) =
  checkPrecedences opers
  where
    opers :: [S.Name]
    opers = mapMaybe P.getPatternAtomIden (toList atoms)

-------------------------------------------------------------------------------
-- Infix Expression
-------------------------------------------------------------------------------

makeExpressionTable ::
  ExpressionAtoms 'Scoped -> [[P.Operator Parse Expression]]
makeExpressionTable (ExpressionAtoms atoms _) = [recordUpdate] : [appOpExplicit] : operators ++ [[functionOp]]
  where
    operators = mkSymbolTable idens

    idens :: [ScopedIden]
    idens = mapMaybe getIden (toList atoms)
      where
        getIden :: ExpressionAtom 'Scoped -> Maybe ScopedIden
        getIden a = case a of
          AtomIdentifier nm -> Just nm
          _ -> Nothing

    mkSymbolTable :: [ScopedIden] -> [[P.Operator Parse Expression]]
    mkSymbolTable = reverse . map (map snd) . groupSortOn' fst . mapMaybe mkOperator
      where
        mkOperator :: ScopedIden -> Maybe (Precedence, P.Operator Parse Expression)
        mkOperator iden
          | Just Fixity {..} <- _nameFixity =
              case _fixityArity of
                OpUnary u -> Just (_fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                  where
                    unaryApp :: ScopedIden -> Expression -> Expression
                    unaryApp funName arg = case u of
                      AssocPostfix -> ExpressionPostfixApplication (PostfixApplication arg funName)
                OpBinary b -> Just (_fixityPrecedence, infixLRN (binaryApp <$> parseSymbolId _nameId))
                  where
                    binaryApp :: ScopedIden -> Expression -> Expression -> Expression
                    binaryApp _infixAppOperator _infixAppLeft _infixAppRight =
                      ExpressionInfixApplication InfixApplication {..}
                    infixLRN :: Parse (Expression -> Expression -> Expression) -> P.Operator Parse Expression
                    infixLRN = case b of
                      AssocLeft -> P.InfixL
                      AssocRight -> P.InfixR
                      AssocNone -> P.InfixN
                OpNone -> Nothing
          | otherwise = Nothing
          where
            S.Name' {..} = iden ^. scopedIdenName

        parseSymbolId :: S.NameId -> Parse ScopedIden
        parseSymbolId uid = P.token getIdentifierWithId mempty
          where
            getIdentifierWithId :: ExpressionAtom 'Scoped -> Maybe ScopedIden
            getIdentifierWithId s = case s of
              AtomIdentifier iden
                | uid == iden ^. scopedIdenName . S.nameId -> Just iden
              _ -> Nothing

    recordUpdate :: P.Operator Parse Expression
    recordUpdate = P.Postfix (mkRecUpdate <$> P.token getRecUpdate mempty)
      where
        getRecUpdate :: ExpressionAtom 'Scoped -> (Maybe (RecordUpdate 'Scoped))
        getRecUpdate = \case
          AtomRecordUpdate u -> Just u
          _ -> Nothing
        mkRecUpdate :: RecordUpdate 'Scoped -> Expression -> Expression
        mkRecUpdate u = ExpressionRecordUpdate . RecordUpdateApp u

    -- Application by juxtaposition.
    appOpExplicit :: P.Operator Parse Expression
    appOpExplicit = P.InfixL (return app)
      where
        app :: Expression -> Expression -> Expression
        app f x =
          ExpressionApplication $
            Application
              { _applicationFunction = f,
                _applicationParameter = x
              }

    -- Non-dependent function type: A → B
    functionOp :: P.Operator Parse Expression
    functionOp = P.InfixR (nonDepFun <$> P.token getArrow mempty)
      where
        getArrow :: ExpressionAtom 'Scoped -> Maybe KeywordRef
        getArrow = \case
          AtomFunArrow r -> return r
          _ -> Nothing

        nonDepFun :: KeywordRef -> Expression -> Expression -> Expression
        nonDepFun _funKw l r =
          ExpressionFunction
            Function
              { _funParameters = params,
                _funReturn = r,
                _funKw
              }
          where
            params =
              let (l', explicitOrInstance, delims') = case l of
                    ExpressionDoubleBraces i ->
                      ( i ^. doubleBracesExpression,
                        ImplicitInstance,
                        Just (i ^. doubleBracesDelims . unIrrelevant)
                      )
                    _ -> (l, Explicit, Nothing)
               in FunctionParameters
                    { _paramNames = [],
                      _paramDelims = Irrelevant delims',
                      _paramColon = Irrelevant Nothing,
                      _paramImplicit = explicitOrInstance,
                      _paramType = l'
                    }

parseExpressionAtoms ::
  forall r.
  (Members '[Error ScoperError, State Scope, InfoTableBuilder] r) =>
  ExpressionAtoms 'Scoped ->
  Sem r Expression
parseExpressionAtoms a@(ExpressionAtoms atoms _) = do
  checkExpressionPrecedences a
  case res of
    Left {} ->
      throw
        ( ErrInfixParser
            InfixError {_infixErrorAtoms = a}
        )
    Right r -> return r
  where
    parser :: Parse Expression
    parser = runM (mkExpressionParser tbl) <* P.eof
    res = P.parse parser filePath (toList atoms)
    tbl = makeExpressionTable a
    filePath :: FilePath
    filePath = ""

-- | Monad for parsing expression sections.
type Parse = P.Parsec () [ExpressionAtom 'Scoped]

mkExpressionParser ::
  [[P.Operator Parse Expression]] ->
  Sem '[Embed Parse] Expression
mkExpressionParser table = embed @Parse pExpression
  where
    pExpression :: Parse Expression
    pExpression = P.makeExprParser (runM parseTerm) table

parseTerm :: (Members '[Embed Parse] r) => Sem r Expression
parseTerm =
  embed @Parse $
    parseUniverse
      <|> parseNoInfixIdentifier
      <|> parseParens
      <|> parseHole
      <|> parseInstanceHole
      <|> parseFunction
      <|> parseLambda
      <|> parseCase
      <|> parseNewCase
      <|> parseList
      <|> parseLiteral
      <|> parseLet
      <|> parseIterator
      <|> parseDoubleBraces
      <|> parseBraces
      <|> parseNamedApplication
  where
    parseHole :: Parse Expression
    parseHole = ExpressionHole <$> P.token lit mempty
      where
        lit :: ExpressionAtom 'Scoped -> Maybe Hole
        lit s = case s of
          AtomHole l -> Just l
          _ -> Nothing

    parseInstanceHole :: Parse Expression
    parseInstanceHole = ExpressionInstanceHole <$> P.token lit mempty
      where
        lit :: ExpressionAtom 'Scoped -> Maybe Hole
        lit s = case s of
          AtomInstanceHole l -> Just l
          _ -> Nothing

    parseLiteral :: Parse Expression
    parseLiteral = ExpressionLiteral <$> P.token lit mempty
      where
        lit :: ExpressionAtom 'Scoped -> Maybe LiteralLoc
        lit s = case s of
          AtomLiteral l -> Just l
          _ -> Nothing

    parseLambda :: Parse Expression
    parseLambda = ExpressionLambda <$> P.token lambda mempty
      where
        lambda :: ExpressionAtom 'Scoped -> Maybe (Lambda 'Scoped)
        lambda s = case s of
          AtomLambda l -> Just l
          _ -> Nothing

    parseCase :: Parse Expression
    parseCase = ExpressionCase <$> P.token case_ mempty
      where
        case_ :: ExpressionAtom 'Scoped -> Maybe (Case 'Scoped)
        case_ s = case s of
          AtomCase l -> Just l
          _ -> Nothing

    parseNewCase :: Parse Expression
    parseNewCase = ExpressionNewCase <$> P.token case_ mempty
      where
        case_ :: ExpressionAtom 'Scoped -> Maybe (NewCase 'Scoped)
        case_ s = case s of
          AtomNewCase l -> Just l
          _ -> Nothing

    parseList :: Parse Expression
    parseList = ExpressionList <$> P.token case_ mempty
      where
        case_ :: ExpressionAtom 'Scoped -> Maybe (List 'Scoped)
        case_ s = case s of
          AtomList l -> Just l
          _ -> Nothing

    parseUniverse :: Parse Expression
    parseUniverse = ExpressionUniverse <$> P.token universe' mempty
      where
        universe' :: ExpressionAtom 'Scoped -> Maybe Universe
        universe' s = case s of
          AtomUniverse u -> Just u
          _ -> Nothing

    parseFunction :: Parse Expression
    parseFunction = ExpressionFunction <$> P.token function mempty
      where
        function :: ExpressionAtom 'Scoped -> Maybe (Function 'Scoped)
        function s = case s of
          AtomFunction u -> Just u
          _ -> Nothing

    parseNamedApplication :: Parse Expression
    parseNamedApplication = ExpressionNamedApplication <$> P.token namedApp mempty
      where
        namedApp :: ExpressionAtom 'Scoped -> Maybe (NamedApplication 'Scoped)
        namedApp s = case s of
          AtomNamedApplication u -> Just u
          _ -> Nothing

    parseLet :: Parse Expression
    parseLet = ExpressionLet <$> P.token letBlock mempty
      where
        letBlock :: ExpressionAtom 'Scoped -> Maybe (Let 'Scoped)
        letBlock s = case s of
          AtomLet u -> Just u
          _ -> Nothing

    parseIterator :: Parse Expression
    parseIterator = ExpressionIterator <$> P.token iterator mempty
      where
        iterator :: ExpressionAtom 'Scoped -> Maybe (Iterator 'Scoped)
        iterator s = case s of
          AtomIterator u -> Just u
          _ -> Nothing

    parseNoInfixIdentifier :: Parse Expression
    parseNoInfixIdentifier = ExpressionIdentifier <$> P.token identifierNoFixity mempty
      where
        identifierNoFixity :: ExpressionAtom 'Scoped -> Maybe ScopedIden
        identifierNoFixity s = case s of
          AtomIdentifier iden
            | not (S.hasFixity (iden ^. scopedIdenName)) -> Just iden
          _ -> Nothing

    parseDoubleBraces :: Parse Expression
    parseDoubleBraces = ExpressionDoubleBraces <$> P.token bracedExpr mempty
      where
        bracedExpr :: ExpressionAtom 'Scoped -> Maybe (DoubleBracesExpression 'Scoped)
        bracedExpr = \case
          AtomDoubleBraces l -> Just l
          _ -> Nothing

    parseBraces :: Parse Expression
    parseBraces = ExpressionBraces <$> P.token bracedExpr mempty
      where
        bracedExpr :: ExpressionAtom 'Scoped -> Maybe (WithLoc Expression)
        bracedExpr = \case
          AtomBraces l -> Just l
          _ -> Nothing

    parseParens :: Parse Expression
    parseParens = P.token parenExpr mempty
      where
        parenExpr :: ExpressionAtom 'Scoped -> Maybe Expression
        parenExpr s = case s of
          AtomParens e -> Just e
          _ -> Nothing

-------------------------------------------------------------------------------
-- Infix Patterns
-------------------------------------------------------------------------------

type ParsePat = P.ParsecT () [PatternAtom 'Scoped] (Sem '[Error ScoperError])

makePatternTable ::
  PatternAtoms 'Scoped -> [[P.Operator ParsePat PatternArg]]
makePatternTable (PatternAtoms latoms _) = [appOp] : operators
  where
    getConstructorRef :: PatternAtom 'Scoped -> Maybe ScopedIden
    getConstructorRef = \case
      PatternAtomIden i -> case i of
        PatternScopedConstructor c -> Just c
        _ -> Nothing
      _ -> Nothing
    operators = mkSymbolTable (mapMaybe getConstructorRef (toList latoms))
    mkSymbolTable :: [ScopedIden] -> [[P.Operator ParsePat PatternArg]]
    mkSymbolTable = reverse . map (map snd) . groupSortOn' fst . mapMaybe unqualifiedSymbolOp
      where
        unqualifiedSymbolOp :: ScopedIden -> Maybe (Precedence, P.Operator ParsePat PatternArg)
        unqualifiedSymbolOp constr = run . runFail $ do
          Fixity {..} <- failMaybe (constr ^. scopedIdenName . S.nameFixity)
          let _nameId = constr ^. scopedIdenName . S.nameId
          case _fixityArity of
            OpUnary u -> return (_fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
              where
                unaryApp :: ScopedIden -> PatternArg -> PatternArg
                unaryApp constrName = case u of
                  AssocPostfix -> explicitP . PatternPostfixApplication . (`PatternPostfixApp` constrName)
            OpBinary b -> return (_fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId _nameId))
              where
                binaryInfixApp :: ScopedIden -> PatternArg -> PatternArg -> PatternArg
                binaryInfixApp name argLeft = explicitP . PatternInfixApplication . PatternInfixApp argLeft name
                infixLRN :: ParsePat (PatternArg -> PatternArg -> PatternArg) -> P.Operator ParsePat PatternArg
                infixLRN = case b of
                  AssocLeft -> P.InfixL
                  AssocRight -> P.InfixR
                  AssocNone -> P.InfixN
            OpNone -> fail
        parseSymbolId :: S.NameId -> ParsePat ScopedIden
        parseSymbolId uid = P.token getConstructorRefWithId mempty
          where
            getConstructorRefWithId :: PatternAtom 'Scoped -> Maybe ScopedIden
            getConstructorRefWithId s = do
              ref <- getConstructorRef s
              guard (ref ^. scopedIdenName . S.nameId == uid)
              return ref

    -- Application by juxtaposition.
    appOp :: P.Operator ParsePat PatternArg
    appOp = P.InfixL (return app)
      where
        app :: PatternArg -> PatternArg -> PatternArg
        app l r =
          explicitP
            ( PatternApplication
                ( PatternApp
                    { _patAppLeft = l,
                      _patAppRight = r
                    }
                )
            )

explicitP :: Pattern -> PatternArg
explicitP _patternArgPattern =
  PatternArg
    { _patternArgIsImplicit = Explicit,
      _patternArgName = Nothing,
      _patternArgPattern
    }

parsePatternTerm ::
  forall r.
  (Members '[Embed ParsePat] r) =>
  Sem r PatternArg
parsePatternTerm = do
  embed @ParsePat $
    parseNoInfixConstructor
      <|> parseVariable
      <|> parseDoubleBraces
      <|> parseParens
      <|> parseBraces
      <|> parseWildcard
      <|> parseEmpty
      <|> parseAt
      <|> parseList
      <|> parseRecord
  where
    parseNoInfixConstructor :: ParsePat PatternArg
    parseNoInfixConstructor =
      explicitP
        . PatternConstructor
        <$> P.token constructorNoFixity mempty
      where
        constructorNoFixity :: PatternAtom 'Scoped -> Maybe ScopedIden
        constructorNoFixity s = case s of
          PatternAtomIden (PatternScopedConstructor ref)
            | not (S.hasFixity (n ^. scopedIdenName)) -> Just ref
            where
              n = ref
          _ -> Nothing

    parseWildcard :: ParsePat PatternArg
    parseWildcard = explicitP . PatternWildcard <$> P.token isWildcard mempty
      where
        isWildcard :: PatternAtom 'Scoped -> Maybe Wildcard
        isWildcard s = case s of
          PatternAtomWildcard i -> Just i
          _ -> Nothing

    parseList :: ParsePat PatternArg
    parseList = explicitP . PatternList <$> P.token isList mempty
      where
        isList :: PatternAtom 'Scoped -> Maybe (ListPattern 'Scoped)
        isList s = case s of
          PatternAtomList i -> Just i
          _ -> Nothing

    parseRecord :: ParsePat PatternArg
    parseRecord = explicitP . PatternRecord <$> P.token isRecord mempty
      where
        isRecord :: PatternAtom 'Scoped -> Maybe (RecordPattern 'Scoped)
        isRecord s = case s of
          PatternAtomRecord i -> Just i
          _ -> Nothing

    parseEmpty :: ParsePat PatternArg
    parseEmpty = explicitP . PatternEmpty <$> P.token isEmpty mempty
      where
        isEmpty :: PatternAtom 'Scoped -> Maybe Interval
        isEmpty s = case s of
          PatternAtomEmpty i -> Just i
          _ -> Nothing

    parseAt :: ParsePat PatternArg
    parseAt = do
      res <- P.token isAt mempty
      case res of
        Left e -> P.lift (throw e)
        Right a -> return a
      where
        isAt :: PatternAtom 'Scoped -> Maybe (Either ScoperError PatternArg)
        isAt = \case
          PatternAtomAt p -> Just $ case p ^. patternArgPattern of
            PatternVariable _ -> Left (ErrAliasBinderPattern (AliasBinderPattern p))
            _ -> Right p
          _ -> Nothing

    parseVariable :: ParsePat PatternArg
    parseVariable = explicitP . PatternVariable <$> P.token var mempty
      where
        var :: PatternAtom 'Scoped -> Maybe S.Symbol
        var s = case s of
          PatternAtomIden (PatternScopedVar sym) -> Just sym
          _ -> Nothing

    parseDoubleBraces :: ParsePat PatternArg
    parseDoubleBraces = do
      res <- P.token bracesPat mempty
      either (P.lift . throw) return res
      where
        bracesPat :: PatternAtom 'Scoped -> Maybe (Either ScoperError PatternArg)
        bracesPat = \case
          PatternAtomDoubleBraces r
            | Implicit <- r ^. patternArgIsImplicit ->
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r True)))
            | ImplicitInstance <- r ^. patternArgIsImplicit ->
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r True)))
            | otherwise -> Just (Right (set patternArgIsImplicit ImplicitInstance r))
          _ -> Nothing

    parseBraces :: ParsePat PatternArg
    parseBraces = do
      res <- P.token bracesPat mempty
      case res of
        Left er -> P.lift (throw er)
        Right a -> return a
      where
        bracesPat :: PatternAtom 'Scoped -> Maybe (Either ScoperError PatternArg)
        bracesPat = \case
          PatternAtomBraces r
            | Implicit <- r ^. patternArgIsImplicit ->
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r False)))
            | ImplicitInstance <- r ^. patternArgIsImplicit ->
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r False)))
            | otherwise -> Just (Right (set patternArgIsImplicit Implicit r))
          _ -> Nothing

    parseParens :: ParsePat PatternArg
    parseParens = P.token parenPat mempty
      where
        parenPat :: PatternAtom 'Scoped -> Maybe PatternArg
        parenPat = \case
          PatternAtomParens r -> Just r
          _ -> Nothing

mkPatternParser ::
  forall r.
  (Members '[Embed ParsePat] r) =>
  [[P.Operator ParsePat PatternArg]] ->
  Sem r PatternArg
mkPatternParser table = embed @ParsePat pPattern
  where
    pPattern :: ParsePat PatternArg
    pPattern = P.makeExprParser pTerm table
    pTerm :: ParsePat PatternArg
    pTerm = runM parseTermRec
      where
        parseTermRec :: Sem '[Embed ParsePat] PatternArg
        parseTermRec = runReader pPattern parsePatternTerm

parsePatternAtom ::
  (Members '[Error ScoperError, State Scope, InfoTableBuilder] r) =>
  PatternAtom 'Scoped ->
  Sem r PatternArg
parsePatternAtom = parsePatternAtoms . singletonAtom
  where
    singletonAtom :: PatternAtom 'Scoped -> PatternAtoms 'Scoped
    singletonAtom a = PatternAtoms (NonEmpty.singleton a) (Irrelevant (getLoc a))

parsePatternAtoms ::
  (Members '[Error ScoperError, State Scope, InfoTableBuilder] r) =>
  PatternAtoms 'Scoped ->
  Sem r PatternArg
parsePatternAtoms atoms@(PatternAtoms sec' _) = do
  checkPatternPrecedences atoms
  case run (runError res) of
    Left e -> throw e -- Scoper effect error
    Right Left {} -> throw (ErrInfixPattern (InfixErrorP atoms)) -- Megaparsec error
    Right (Right r) -> return r
  where
    sec = toList sec'
    tbl = makePatternTable atoms
    parser :: ParsePat PatternArg
    parser = runM (mkPatternParser tbl) <* P.eof
    res = P.runParserT parser filePath sec

    filePath :: FilePath
    filePath = "tmp"
