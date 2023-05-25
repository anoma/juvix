-- | Limitations:
-- 1. A symbol introduced by a type signature can only be used once per Module.
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
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.InfoTableBuilder
import Juvix.Compiler.Concrete.Data.Name qualified as N
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context (ParserResult)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

iniScoperState :: ScoperState
iniScoperState =
  ScoperState
    { _scoperModulesCache = ModulesCache mempty,
      _scoperModules = mempty,
      _scoperScope = mempty
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

scopeCheckExpression ::
  forall r.
  (Members '[Error JuvixError, NameIdGen, State Scope] r) =>
  InfoTable ->
  ExpressionAtoms 'Parsed ->
  Sem r Expression
scopeCheckExpression tab as = mapError (JuvixError @ScoperError) $ do
  snd
    <$> ( ignoreHighlightBuilder
            . runInfoTableBuilder tab
            . runReader iniScopeParameters
            . evalState iniScoperState
            . withLocalScope
            $ checkParseExpressionAtoms as
        )
  where
    iniScopeParameters :: ScopeParameters
    iniScopeParameters =
      ScopeParameters
        { _scopeTopParents = mempty,
          _scopeParsedModules = mempty
        }

checkParseExpressionAtoms' ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms' = checkExpressionAtoms >=> parseExpressionAtoms

scopeCheckImport ::
  forall r.
  Members '[Error JuvixError, InfoTableBuilder, NameIdGen, State Scope, Reader ScopeParameters, State ScoperState] r =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
scopeCheckImport i = mapError (JuvixError @ScoperError) $ checkImport i

scopeCheckOpenModule ::
  forall r.
  Members '[Error JuvixError, InfoTableBuilder, NameIdGen, State Scope, Reader ScopeParameters, State ScoperState] r =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
scopeCheckOpenModule i = mapError (JuvixError @ScoperError) $ checkOpenModule i

freshVariable :: Members '[NameIdGen, State ScoperFixities, State Scope, State ScoperState] r => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol S.KNameLocal

freshSymbol ::
  forall r.
  (Members '[State Scope, State ScoperState, NameIdGen, State ScoperFixities] r) =>
  S.NameKind ->
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
  return S.Name' {..}
  where
    fixity :: Sem r (Maybe Fixity)
    fixity
      | S.canHaveFixity _nameKind = do
          mf <- gets (^? scoperFixities . at _nameConcrete . _Just . symbolFixityDef . opFixity)
          when (isJust mf) (modify (set (scoperFixities . at _nameConcrete . _Just . symbolFixityUsed) True))
          return mf
      | otherwise = return Nothing

reserveSymbolOf ::
  forall r.
  (Members '[Error ScoperError, NameIdGen, State ScoperFixities, State Scope, State ScoperState] r) =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolOf k s = do
  checkNotBound
  freshSymbol k s
  where
    checkNotBound :: Sem r ()
    checkNotBound = do
      exists <- HashMap.lookup s <$> gets (^. scopeLocalSymbols)
      whenJust exists $ \d ->
        throw
          ( ErrMultipleDeclarations
              MultipleDeclarations
                { _multipleDeclSecond = s,
                  _multipleDeclFirst = getLoc d
                }
          )

bindReservedSymbol :: Members '[State Scope, InfoTableBuilder, Reader BindingStrategy] r => S.Symbol -> SymbolEntry -> Sem r ()
bindReservedSymbol s' entry = do
  path <- gets (^. scopePath)
  strat <- ask
  modify (over scopeSymbols (HashMap.alter (Just . addS strat path) s))
  modify (set (scopeLocalSymbols . at s) (Just s'))
  registerName (S.unqualifiedSymbol s')
  where
    s :: Symbol
    s = s' ^. S.nameConcrete
    addS :: BindingStrategy -> S.AbsModulePath -> Maybe SymbolInfo -> SymbolInfo
    addS strat path m = case m of
      Nothing -> symbolInfoSingle entry
      Just SymbolInfo {..} -> case strat of
        BindingLocal -> symbolInfoSingle entry
        BindingTop -> SymbolInfo (HashMap.insert path entry _symbolInfo)

bindSymbolOf ::
  (Members '[Error ScoperError, NameIdGen, State ScoperFixities, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r) =>
  S.NameKind ->
  (S.Name' () -> SymbolEntry) ->
  Symbol ->
  Sem r S.Symbol
bindSymbolOf k mkEntry s = do
  s' <- reserveSymbolOf k s
  bindReservedSymbol s' (mkEntry (S.unConcrete s'))
  return s'

ignoreFixities :: Sem (State ScoperFixities ': r) a -> Sem r a
ignoreFixities = evalState mempty

-- variables are assumed to never be infix operators
bindVariableSymbol ::
  Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder, State ScoperState] r =>
  Symbol ->
  Sem r S.Symbol
bindVariableSymbol = localBindings . ignoreFixities . bindSymbolOf S.KNameLocal EntryVariable

bindFunctionSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
bindFunctionSymbol = bindSymbolOf S.KNameFunction (EntryFunction . FunctionRef')

bindInductiveSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
bindInductiveSymbol = bindSymbolOf S.KNameInductive (EntryInductive . InductiveRef')

bindAxiomSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
bindAxiomSymbol = bindSymbolOf S.KNameAxiom (EntryAxiom . AxiomRef')

bindConstructorSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
bindConstructorSymbol = bindSymbolOf S.KNameConstructor (EntryConstructor . ConstructorRef')

bindLocalModuleSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  ExportInfo ->
  Module 'Scoped 'ModuleLocal ->
  Symbol ->
  Sem r S.Symbol
bindLocalModuleSymbol _moduleExportInfo _moduleRefModule =
  bindSymbolOf
    S.KNameLocalModule
    (\_moduleRefName -> EntryModule (mkModuleRef' (ModuleRef'' {..})))

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

-- | Do not call directly. Looks for a symbol in (possibly) nested local modules
lookupSymbolAux ::
  forall r.
  Members '[State Scope] r =>
  [Symbol] ->
  Symbol ->
  Sem r [SymbolEntry]
lookupSymbolAux modules final = do
  local' <- hereOrInLocalModule
  import' <- importedTopModule
  return (local' ++ import')
  where
    hereOrInLocalModule :: Sem r [SymbolEntry] =
      case modules of
        [] -> do
          r <- HashMap.lookup final <$> gets (^. scopeSymbols)
          return $ case r of
            Nothing -> []
            Just SymbolInfo {..} -> toList _symbolInfo
        p : ps ->
          mapMaybe (lookInExport final ps . getModuleExportInfo)
            . concat
            . maybeToList
            . fmap (mapMaybe getModuleRef . toList . (^. symbolInfo))
            . HashMap.lookup p
            <$> gets (^. scopeSymbols)
    importedTopModule :: Sem r [SymbolEntry]
    importedTopModule = do
      tbl <- gets (^. scopeTopModules)
      return (tbl ^.. at path . _Just . each . to (EntryModule . mkModuleRef'))
      where
        path = TopModulePath modules final

lookInExport :: Symbol -> [Symbol] -> ExportInfo -> Maybe SymbolEntry
lookInExport sym remaining e = case remaining of
  [] -> HashMap.lookup sym (e ^. exportSymbols)
  (s : ss) -> do
    export <- mayModule e s
    lookInExport sym ss export
  where
    mayModule :: ExportInfo -> Symbol -> Maybe ExportInfo
    mayModule ExportInfo {..} s = do
      entry <- HashMap.lookup s _exportSymbols
      case entry of
        EntryModule m -> Just (getModuleExportInfo m)
        _ -> Nothing

-- | We return a list of entries because qualified names can point to different
-- modules due to nesting.
lookupQualifiedSymbol ::
  forall r.
  Members '[State Scope] r =>
  ([Symbol], Symbol) ->
  Sem r [SymbolEntry]
lookupQualifiedSymbol (path, sym) = do
  here' <- here
  there' <- there
  return (here' ++ there')
  where
    -- Current module.
    here :: Sem r [SymbolEntry]
    here = lookupSymbolAux path sym
    -- Looks for a top level modules
    there :: Sem r [SymbolEntry]
    there = do
      concatMapM (uncurry lookInTopModule) allTopPaths
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
        lookInTopModule :: TopModulePath -> [Symbol] -> Sem r [SymbolEntry]
        lookInTopModule topPath remaining = do
          tbl <- gets (^. scopeTopModules)
          return $
            catMaybes
              [ lookInExport sym remaining (ref ^. moduleExportInfo)
                | Just t <- [tbl ^. at topPath],
                  ref <- toList t
              ]

checkQualifiedExpr ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  QualifiedName ->
  Sem r ScopedIden
checkQualifiedExpr q@(QualifiedName (SymbolPath p) sym) = do
  es <- filter entryIsExpression <$> lookupQualifiedSymbol (toList p, sym)
  case es of
    [] -> notInScope
    [e] -> entryToScopedIden q' e
    _ -> throw (ErrAmbiguousSym (AmbiguousSym q' es))
  where
    q' = NameQualified q
    notInScope = throw (ErrQualSymNotInScope (QualSymNotInScope q))

entryToScopedIden :: (Members '[InfoTableBuilder] r) => Name -> SymbolEntry -> Sem r ScopedIden
entryToScopedIden name e = do
  let scopedName :: S.Name
      scopedName = set S.nameConcrete name (entryName e)
  registerName scopedName
  return $ case e of
    EntryAxiom ref -> ScopedAxiom (set (axiomRefName . S.nameConcrete) name ref)
    EntryInductive ref ->
      ScopedInductive (set (inductiveRefName . S.nameConcrete) name ref)
    EntryConstructor ref ->
      ScopedConstructor (set (constructorRefName . S.nameConcrete) name ref)
    EntryFunction ref ->
      ScopedFunction (set (functionRefName . S.nameConcrete) name ref)
    EntryVariable v -> case name of
      NameQualified {} -> impossible
      NameUnqualified uname -> ScopedVar (set S.nameConcrete uname v)
    EntryModule {} -> impossible

-- | We gather all symbols which have been defined or marked to be public in the given scope.
exportScope :: forall r. (Members '[State Scope, Error ScoperError] r) => Scope -> Sem r ExportInfo
exportScope Scope {..} = do
  _exportSymbols <- getExportSymbols
  return ExportInfo {..}
  where
    getExportSymbols :: Sem r (HashMap Symbol SymbolEntry)
    getExportSymbols = HashMap.fromList <$> mapMaybeM entry (HashMap.toList _scopeSymbols)
      where
        shouldExport :: SymbolEntry -> Bool
        shouldExport ent = _nameVisibilityAnn == VisPublic
          where
            S.Name' {..} = entryName ent

        entry :: (Symbol, SymbolInfo) -> Sem r (Maybe (Symbol, SymbolEntry))
        entry (s, SymbolInfo {..}) =
          case filter shouldExport (toList _symbolInfo) of
            [] -> return Nothing
            [e] -> return $ Just (s, e)
            (e : es) ->
              throw
                ( ErrMultipleExport
                    (MultipleExportConflict _scopePath s (e :| es))
                )

getParsedModule :: Members '[Reader ScopeParameters] r => TopModulePath -> Sem r (Module 'Parsed 'ModuleTop)
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

checkOperatorSyntaxDef ::
  forall r.
  Members '[Error ScoperError, State Scope, State ScoperState, State ScoperFixities] r =>
  OperatorSyntaxDef ->
  Sem r ()
checkOperatorSyntaxDef s@OperatorSyntaxDef {..} = do
  checkNotDefined
  let sf =
        SymbolFixity
          { _symbolFixityUsed = False,
            _symbolFixityDef = s
          }
  modify (over scoperFixities (HashMap.insert _opSymbol sf))
  where
    checkNotDefined :: Sem r ()
    checkNotDefined =
      whenJustM
        (HashMap.lookup _opSymbol <$> gets (^. scoperFixities))
        $ \s' -> throw (ErrDuplicateFixity (DuplicateFixity (s' ^. symbolFixityDef) s))

-- | Only used as syntactical convenience for registerX functions
(@$>) :: Functor m => (a -> m ()) -> a -> m a
(@$>) f a = f a $> a

checkTypeSignature ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities, Reader BindingStrategy] r) =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature TypeSignature {..} = do
  sigType' <- checkParseExpressionAtoms _sigType
  sigName' <- bindFunctionSymbol _sigName
  sigDoc' <- mapM checkJudoc _sigDoc
  sigBody' <- mapM checkParseExpressionAtoms _sigBody
  registerTypeSignature
    @$> TypeSignature
      { _sigName = sigName',
        _sigType = sigType',
        _sigDoc = sigDoc',
        _sigBody = sigBody',
        ..
      }

checkInductiveParameters ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  InductiveParameters 'Parsed ->
  Sem r (InductiveParameters 'Scoped)
checkInductiveParameters params = do
  _inductiveParametersType <- checkParseExpressionAtoms (params ^. inductiveParametersType)
  _inductiveParametersNames <- mapM bindVariableSymbol (params ^. inductiveParametersNames)
  return InductiveParameters {..}

checkInductiveDef ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities] r) =>
  InductiveDef 'Parsed ->
  Sem r (InductiveDef 'Scoped)
checkInductiveDef InductiveDef {..} = do
  inductiveName' <- topBindings (bindInductiveSymbol _inductiveName)
  (inductiveParameters', inductiveType', inductiveDoc', inductiveConstructors') <- withLocalScope $ do
    inductiveParameters' <- mapM checkInductiveParameters _inductiveParameters
    inductiveType' <- mapM checkParseExpressionAtoms _inductiveType
    inductiveDoc' <- mapM checkJudoc _inductiveDoc
    inductiveConstructors' <- mapM checkConstructorDef _inductiveConstructors
    return (inductiveParameters', inductiveType', inductiveDoc', inductiveConstructors')
  forM_ inductiveConstructors' bindConstructor
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
        _inductiveKw
      }
  where
    bindConstructor :: InductiveConstructorDef 'Scoped -> Sem r ()
    bindConstructor d =
      topBindings $
        bindReservedSymbol
          (d ^. constructorName)
          ( EntryConstructor
              ( ConstructorRef'
                  (S.unConcrete (d ^. constructorName))
              )
          )
    -- note that the constructor name is not bound here
    checkConstructorDef :: InductiveConstructorDef 'Parsed -> Sem r (InductiveConstructorDef 'Scoped)
    checkConstructorDef InductiveConstructorDef {..} = do
      constructorType' <- checkParseExpressionAtoms _constructorType
      constructorName' <- reserveSymbolOf S.KNameConstructor _constructorName
      doc' <- mapM checkJudoc _constructorDoc
      registerConstructor
        @$> InductiveConstructorDef
          { _constructorName = constructorName',
            _constructorType = constructorType',
            _constructorDoc = doc',
            _constructorPragmas = _constructorPragmas,
            _constructorPipe
          }

createExportsTable :: ExportInfo -> HashSet NameId
createExportsTable ei = foldr (HashSet.insert . getNameId) HashSet.empty (HashMap.elems (ei ^. exportSymbols))
  where
    getNameId :: SymbolEntry -> NameId
    getNameId = \case
      EntryAxiom r -> getNameRefId (r ^. axiomRefName)
      EntryInductive r -> getNameRefId (r ^. inductiveRefName)
      EntryFunction r -> getNameRefId (r ^. functionRefName)
      EntryConstructor r -> getNameRefId (r ^. constructorRefName)
      EntryModule r -> getModuleRefNameId r
      EntryVariable v -> v ^. S.nameId

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
          _nameKind = S.KNameTopModule
          _nameFixity :: Maybe Fixity
          _nameFixity = Nothing
          -- This visibility annotation is not relevant
          _nameVisibilityAnn = VisPublic
          _nameWhyInScope = S.BecauseDefined
          _nameVerbatim = N.topModulePathToDottedPath _modulePath
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
                    _moduleKwEnd
                  }
              _moduleRefName = S.unConcrete path'
          return (ModuleRef'' {..}, path')
      modify (set (scoperScope . at (p ^. S.nameConcrete)) (Just s))
      return m'

withTopScope :: Members '[State Scope] r => Sem r a -> Sem r a
withTopScope ma = do
  before <- get @Scope
  let scope' = set scopeLocalSymbols mempty before
  put scope'
  ma

withLocalScope :: Members '[State Scope] r => Sem r a -> Sem r a
withLocalScope ma = do
  before <- get @Scope
  let scope' = set scopeLocalSymbols mempty before
  put scope'
  x <- ma
  put before
  return x

fixitiesBlock :: Members '[Error ScoperError] r => Sem (State ScoperFixities ': r) a -> Sem r a
fixitiesBlock m =
  evalState (mempty :: ScoperFixities) $ do
    a <- m
    checkOrphanFixities
    return a

checkModuleBody ::
  forall r.
  Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r =>
  [Statement 'Parsed] ->
  Sem r (ExportInfo, [Statement 'Scoped])
checkModuleBody body = do
  body' <- fixitiesBlock (mapM checkStatement body)
  exported <- get >>= exportScope
  return (exported, body')

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
  _modulePath' <- ignoreFixities (reserveSymbolOf S.KNameLocalModule _modulePath)
  let moduleId = _modulePath' ^. S.nameId
      _moduleRefName = S.unConcrete _modulePath'
      _moduleRefModule =
        Module
          { _modulePath = _modulePath',
            _moduleBody = moduleBody',
            _moduleDoc = moduleDoc',
            _modulePragmas = _modulePragmas,
            _moduleKw,
            _moduleKwEnd
          }
      entry :: ModuleRef' 'S.NotConcrete
      entry = mkModuleRef' @'ModuleLocal ModuleRef'' {..}
  bindReservedSymbol _modulePath' (EntryModule entry)
  registerName (S.unqualifiedSymbol _modulePath')
  modify (over scoperModules (HashMap.insert moduleId entry))
  return _moduleRefModule
  where
    inheritScope :: Sem r ()
    inheritScope = do
      absPath <- (S.<.> _modulePath) <$> gets (^. scopePath)
      modify (set scopePath absPath)
      modify (over scopeSymbols (fmap inheritSymbol))
      where
        inheritSymbol :: SymbolInfo -> SymbolInfo
        inheritSymbol (SymbolInfo s) = SymbolInfo (fmap inheritEntry s)

        inheritEntry :: SymbolEntry -> SymbolEntry
        inheritEntry = entryOverName (over S.nameWhyInScope S.BecauseInherited . set S.nameVisibilityAnn VisPrivate)

checkOrphanFixities :: forall r. Members '[Error ScoperError, State ScoperFixities] r => Sem r ()
checkOrphanFixities = do
  declared <- gets (^. scoperFixities)
  let unused = fmap (^. symbolFixityDef) . find (^. symbolFixityUsed . to not) . toList $ declared
  case unused of
    Nothing -> return ()
    Just x -> throw (ErrUnusedOperatorDef (UnusedOperatorDef x))

symbolInfoSingle :: SymbolEntry -> SymbolInfo
symbolInfoSingle p = SymbolInfo $ HashMap.singleton (entryName p ^. S.nameDefinedIn) p

lookupModuleSymbol ::
  (Members '[Error ScoperError, State Scope, State ScoperState] r) =>
  Name ->
  Sem r ModuleRef
lookupModuleSymbol n = do
  es <- lookupQualifiedSymbol (path, sym)
  case mapMaybe getModuleRef es of
    [] -> notInScope
    [x] -> return (overModuleRef'' (set (moduleRefName . S.nameConcrete) n) x)
    _ -> throw (ErrAmbiguousModuleSym (AmbiguousModuleSym n es))
  where
    notInScope = throw (ErrModuleNotInScope (ModuleNotInScope n))
    (path, sym) = case n of
      NameUnqualified s -> ([], s)
      NameQualified (QualifiedName (SymbolPath p) s) -> (toList p, s)

getModuleRef :: SymbolEntry -> Maybe (ModuleRef' 'S.NotConcrete)
getModuleRef = \case
  EntryModule m -> Just m
  _ -> Nothing

getExportInfo ::
  forall r.
  (Members '[State ScoperState] r) =>
  S.ModuleNameId ->
  Sem r ExportInfo
getExportInfo modId = do
  l <-
    HashMap.lookupDefault impossible modId
      <$> gets (^. scoperModules)
  return $ case l ^. unModuleRef' of
    _ :&: ent -> ent ^. moduleExportInfo

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
            scopedOpen <- checkOpenModuleNoImport op'
            return
              scopedOpen
                { _openModuleImportKw = Just k,
                  _openModuleName = project (import' ^. importModule),
                  _openImportAsName =
                    if
                        | Just asTxt <- (op ^. openImportAsName) -> Just (set S.nameConcrete asTxt topName)
                        | otherwise -> Nothing
                }
  | otherwise = impossible

checkOpenModuleNoImport ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenModuleNoImport OpenModule {..}
  | isJust _openModuleImportKw = impossible
  | otherwise = do
      openModuleName'@(ModuleRef' (_ :&: moduleRef'')) <- lookupModuleSymbol _openModuleName
      let exportInfo@(ExportInfo tbl) = moduleRef'' ^. moduleExportInfo
      registerName (moduleRef'' ^. moduleRefName)

      let checkUsingHiding :: UsingHiding 'Parsed -> Sem r (UsingHiding 'Scoped)
          checkUsingHiding = \case
            Hiding h -> Hiding <$> mapM scopeSymbol h
            Using uh -> Using <$> mapM checkUsingItem uh
            where
              scopeSymbol :: Symbol -> Sem r S.Symbol
              scopeSymbol s = do
                let mentry :: Maybe SymbolEntry
                    mentry = tbl ^. at s
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
                registerName (S.unqualifiedSymbol scopedSym)
                return scopedSym

              checkUsingItem :: UsingItem 'Parsed -> Sem r (UsingItem 'Scoped)
              checkUsingItem i = do
                scopedSym <- scopeSymbol (i ^. usingSymbol)
                let scopedAs = do
                      c <- i ^. usingAs
                      return (set S.nameConcrete c scopedSym)
                mapM_ (registerName . S.unqualifiedSymbol) scopedAs
                return
                  UsingItem
                    { _usingSymbol = scopedSym,
                      _usingAs = scopedAs
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
    mergeScope ExportInfo {..} =
      mapM_ mergeSymbol (HashMap.toList _exportSymbols)
      where
        mergeSymbol :: (Symbol, SymbolEntry) -> Sem r ()
        mergeSymbol (s, entry) =
          modify
            (over scopeSymbols (HashMap.insertWith (<>) s (symbolInfoSingle entry)))

    alterScope :: Maybe (UsingHiding 'Scoped) -> ExportInfo -> ExportInfo
    alterScope openModif = alterEntries . filterScope
      where
        alterEntry :: SymbolEntry -> SymbolEntry
        alterEntry =
          entryOverName
            ( set S.nameWhyInScope S.BecauseImportedOpened
                . set S.nameVisibilityAnn (publicAnnToVis _openPublic)
            )
        alterEntries :: ExportInfo -> ExportInfo
        alterEntries = over exportSymbols (fmap alterEntry)

        publicAnnToVis :: PublicAnn -> VisibilityAnn
        publicAnnToVis = \case
          Public -> VisPublic
          NoPublic -> VisPrivate

        filterScope :: ExportInfo -> ExportInfo
        filterScope = case openModif of
          Just (Using l) -> over exportSymbols (HashMap.fromList . mapMaybe inUsing . HashMap.toList)
            where
              inUsing :: (Symbol, SymbolEntry) -> Maybe (Symbol, SymbolEntry)
              inUsing (sym, e) = do
                mayAs' <- u ^. at (symbolEntryNameId e)
                return (fromMaybe sym mayAs', e)
              u :: HashMap NameId (Maybe Symbol)
              u = HashMap.fromList [(i ^. usingSymbol . S.nameId, i ^? usingAs . _Just . S.nameConcrete) | i <- toList l]
          Just (Hiding l) -> over exportSymbols (HashMap.filter (not . inHiding))
            where
              inHiding :: SymbolEntry -> Bool
              inHiding e = HashSet.member (symbolEntryNameId e) u
              u :: HashSet NameId
              u = HashSet.fromList (map (^. S.nameId) (toList l))
          Nothing -> id

checkOpenModule ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenModule op
  | isJust (op ^. openModuleImportKw) = checkOpenImportModule op
  | otherwise = checkOpenModuleNoImport op

checkFunctionClause ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r) =>
  FunctionClause 'Parsed ->
  Sem r (FunctionClause 'Scoped)
checkFunctionClause clause@FunctionClause {..} = do
  clauseOwnerFunction' <- checkTypeSigInScope
  registerName (S.unqualifiedSymbol clauseOwnerFunction')
  (clausePatterns', clauseBody') <- withLocalScope $ do
    clp <- mapM checkParsePatternAtom _clausePatterns
    clb <- checkParseExpressionAtoms _clauseBody
    return (clp, clb)
  registerFunctionClause
    @$> FunctionClause
      { _clauseOwnerFunction = clauseOwnerFunction',
        _clausePatterns = clausePatterns',
        _clauseBody = clauseBody'
      }
  where
    fun = _clauseOwnerFunction
    checkTypeSigInScope :: Sem r S.Symbol
    checkTypeSigInScope = do
      ms <- HashMap.lookup fun <$> gets (^. scopeLocalSymbols)
      sym <- maybe err return ms
      unless (S.isFunctionKind sym) err
      return (set S.nameConcrete _clauseOwnerFunction sym)
      where
        err :: Sem r a
        err = throw (ErrLacksTypeSig (LacksTypeSig clause))

checkAxiomDef ::
  (Members '[InfoTableBuilder, Error ScoperError, State Scope, State ScoperState, NameIdGen, State ScoperFixities, Reader BindingStrategy] r) =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiomDef AxiomDef {..} = do
  axiomType' <- withLocalScope (checkParseExpressionAtoms _axiomType)
  axiomName' <- bindAxiomSymbol _axiomName
  axiomDoc' <- withLocalScope (mapM checkJudoc _axiomDoc)
  registerAxiom @$> AxiomDef {_axiomName = axiomName', _axiomType = axiomType', _axiomDoc = axiomDoc', ..}

entryToSymbol :: SymbolEntry -> Symbol -> S.Symbol
entryToSymbol sentry csym = set S.nameConcrete csym (symbolEntryToSName sentry)

checkEval ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r) =>
  Eval 'Parsed ->
  Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> withLocalScope (checkParseExpressionAtoms s)

checkPrint ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r) =>
  Print 'Parsed ->
  Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> withLocalScope (checkParseExpressionAtoms s)

checkFunction ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction f = do
  _paramType <- checkParseExpressionAtoms (f ^. funParameters . paramType)
  withLocalScope $ do
    _paramNames <- forM (f ^. funParameters . paramNames) $ \case
      Nothing -> return Nothing
      Just p -> Just <$> bindVariableSymbol p
    _funReturn <- checkParseExpressionAtoms (f ^. funReturn)
    let _paramImplicit = f ^. funParameters . paramImplicit
        _funParameters = FunctionParameters {..}
        _funKw = f ^. funKw
    return Function {..}

-- for now functions defined in let clauses cannot be infix operators
checkLetClause ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  LetClause 'Parsed ->
  Sem r (LetClause 'Scoped)
checkLetClause lc = localBindings . ignoreFixities $ case lc of
  LetTypeSig t -> LetTypeSig <$> checkTypeSignature t
  LetFunClause c -> LetFunClause <$> checkFunctionClause c

checkLetBlock ::
  forall r.
  Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  LetBlock 'Parsed ->
  Sem r (LetBlock 'Scoped)
checkLetBlock LetBlock {..} =
  withLocalScope $ do
    -- local definitions should not stay in scope
    letClauses' <- checkLetClauses _letClauses
    letExpression' <- checkParseExpressionAtoms _letExpression
    return
      LetBlock
        { _letClauses = letClauses',
          _letExpression = letExpression',
          _letKw
        }
  where
    checkLetClauses :: NonEmpty (LetClause 'Parsed) -> Sem r (NonEmpty (LetClause 'Scoped))
    checkLetClauses = mapM checkLetClause

checkCaseBranch ::
  forall r.
  Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  CaseBranch 'Parsed ->
  Sem r (CaseBranch 'Scoped)
checkCaseBranch CaseBranch {..} = withLocalScope $ do
  pattern' <- checkParsePatternAtoms _caseBranchPattern
  checkNotImplicit pattern'
  expression' <- (checkParseExpressionAtoms _caseBranchExpression)
  return $
    CaseBranch
      { _caseBranchPattern = pattern',
        _caseBranchExpression = expression',
        ..
      }
  where
    checkNotImplicit :: PatternArg -> Sem r ()
    checkNotImplicit p =
      when
        (p ^. patternArgIsImplicit == Implicit)
        (throw (ErrCaseBranchImplicitPattern (CaseBranchImplicitPattern p)))

checkCase ::
  Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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

checkLambda ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda Lambda {..} = Lambda _lambdaKw <$> mapM checkLambdaClause _lambdaClauses

checkLambdaClause ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  LambdaClause 'Parsed ->
  Sem r (LambdaClause 'Scoped)
checkLambdaClause LambdaClause {..} = withLocalScope $ do
  lambdaParameters' <- mapM checkParsePatternAtom _lambdaParameters
  lambdaBody' <- checkParseExpressionAtoms _lambdaBody
  return
    LambdaClause
      { _lambdaParameters = lambdaParameters',
        _lambdaBody = lambdaBody',
        _lambdaPipe
      }

scopedVar ::
  (Members '[InfoTableBuilder] r) =>
  LocalVariable ->
  Symbol ->
  Sem r S.Symbol
scopedVar s n = do
  let scoped = set S.nameConcrete n s
  registerName (S.unqualifiedSymbol scoped)
  return scoped

scopedFunction ::
  (Members '[InfoTableBuilder] r) =>
  FunctionRef' 'S.NotConcrete ->
  Symbol ->
  Sem r FunctionRef
scopedFunction (FunctionRef' fref) n = do
  let scoped :: S.Name = set S.nameConcrete (NameUnqualified n) fref
  registerName scoped
  return (FunctionRef' scoped)

checkUnqualified ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  Symbol ->
  Sem r ScopedIden
checkUnqualified s = do
  scope <- get
  -- Lookup at the global scope
  let err = throw (ErrSymNotInScope (NotInScope s scope))
  entries <-
    filter S.isExprKind
      <$> lookupQualifiedSymbol ([], s)
  case resolveShadowing entries of
    [] -> err
    [x] -> entryToScopedIden n x
    es -> throw (ErrAmbiguousSym (AmbiguousSym n es))
  where
    n = NameUnqualified s

-- | Remove the symbol entries associated with a single symbol according to the
-- shadowing rules for modules. For example, a symbol defined in the outer
-- module with the same name as a symbol defined in the inner module will be
-- removed.
resolveShadowing :: [SymbolEntry] -> [SymbolEntry]
resolveShadowing es = go [(e, entryName e ^. S.nameWhyInScope) | e <- es]
  where
    go :: [(SymbolEntry, S.WhyInScope)] -> [SymbolEntry]
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
    Just constr -> do
      registerName (constr ^. constructorRefName)
      return (PatternScopedConstructor constr) -- the symbol is a constructor
    Nothing -> PatternScopedVar <$> bindVariableSymbol sym -- the symbol is a variable
  where
    (path, sym) = case n of
      NameQualified (QualifiedName (SymbolPath p) s) -> (toList p, s)
      NameUnqualified s -> ([], s)
    -- check whether the symbol is a constructor in scope
    getConstructorRef :: Sem r (Maybe ConstructorRef)
    getConstructorRef = do
      entries <- mapMaybe getConstructor <$> lookupQualifiedSymbol (path, sym)
      case entries of
        [] -> case SymbolPath <$> nonEmpty path of
          Nothing -> return Nothing -- There is no constructor with such a name
          Just pth -> throw (ErrQualSymNotInScope (QualSymNotInScope (QualifiedName pth sym)))
        [e] -> return (Just (set (constructorRefName . S.nameConcrete) n e)) -- There is one constructor with such a name
        es -> throw (ErrAmbiguousSym (AmbiguousSym n (map EntryConstructor es)))
    getConstructor :: SymbolEntry -> Maybe (ConstructorRef' 'S.NotConcrete)
    getConstructor = \case
      EntryConstructor r -> Just r
      _ -> Nothing

checkPatternBinding ::
  Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  PatternBinding ->
  Sem r PatternArg
checkPatternBinding (PatternBinding n p) = do
  n' <- bindVariableSymbol n
  p' <- checkParsePatternAtom p
  if
      | isJust (p' ^. patternArgName) -> throw (ErrDoubleBinderPattern (DoubleBinderPattern n' p'))
      | otherwise -> return $ set patternArgName (Just n') p'

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
  PatternAtomAt p -> PatternAtomAt <$> checkPatternBinding p

checkName ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  Name ->
  Sem r ScopedIden
checkName n = case n of
  NameQualified q -> checkQualifiedExpr q
  NameUnqualified s -> checkUnqualified s

checkExpressionAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtom 'Parsed ->
  Sem r (ExpressionAtom 'Scoped)
checkExpressionAtom e = case e of
  AtomIdentifier n -> AtomIdentifier <$> checkName n
  AtomLambda lam -> AtomLambda <$> checkLambda lam
  AtomCase c -> AtomCase <$> checkCase c
  AtomLetBlock letBlock -> AtomLetBlock <$> checkLetBlock letBlock
  AtomUniverse uni -> return (AtomUniverse uni)
  AtomFunction fun -> AtomFunction <$> checkFunction fun
  AtomParens par -> AtomParens <$> checkParens par
  AtomBraces br -> AtomBraces <$> traverseOf withLocParam checkParseExpressionAtoms br
  AtomFunArrow a -> return (AtomFunArrow a)
  AtomHole h -> AtomHole <$> checkHole h
  AtomLiteral l -> return (AtomLiteral l)

checkHole ::
  (Members '[NameIdGen] r) =>
  HoleType 'Parsed ->
  Sem r Hole
checkHole h = do
  i <- freshNameId
  return
    Hole
      { _holeId = i,
        _holeLoc = h
      }

checkParens ::
  Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParens e@(ExpressionAtoms as _) = case as of
  AtomIdentifier s :| [] -> do
    scopedId <- checkName s
    let scopedIdenNoFix = idenOverName (set S.nameFixity Nothing) scopedId
    return (ExpressionParensIdentifier scopedIdenNoFix)
  AtomCase c :| [] -> ExpressionCase . set caseParens True <$> checkCase c
  _ -> checkParseExpressionAtoms e

checkExpressionAtoms ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l i) = (`ExpressionAtoms` i) <$> mapM checkExpressionAtom l

checkJudoc ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Judoc 'Parsed ->
  Sem r (Judoc 'Scoped)
checkJudoc (Judoc groups) = Judoc <$> mapM checkJudocGroup groups

checkJudocGroup ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocGroup 'Parsed ->
  Sem r (JudocGroup 'Scoped)
checkJudocGroup = \case
  JudocGroupBlock b -> JudocGroupBlock <$> checkJudocBlockParagraph b
  JudocGroupLines l -> JudocGroupLines <$> mapM checkJudocBlock l

checkJudocBlock ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocBlock 'Parsed ->
  Sem r (JudocBlock 'Scoped)
checkJudocBlock = \case
  JudocParagraphLines l -> JudocParagraphLines <$> mapM checkJudocLine l
  JudocExample e -> JudocExample <$> traverseOf exampleExpression checkParseExpressionAtoms e

checkJudocBlockParagraph ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocBlockParagraph 'Parsed ->
  Sem r (JudocBlockParagraph 'Scoped)
checkJudocBlockParagraph = traverseOf judocBlockParagraphBlocks (mapM checkJudocBlock)

checkJudocLine ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocParagraphLine 'Parsed ->
  Sem r (JudocParagraphLine 'Scoped)
checkJudocLine (JudocParagraphLine atoms) = JudocParagraphLine <$> mapM (mapM checkJudocAtom) atoms

checkJudocAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  JudocAtom 'Parsed ->
  Sem r (JudocAtom 'Scoped)
checkJudocAtom = \case
  JudocText t -> return (JudocText t)
  JudocExpression e -> JudocExpression <$> checkParseExpressionAtoms e

checkParseExpressionAtoms ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternAtom 'Parsed ->
  Sem r PatternArg
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkStatement ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities] r) =>
  Statement 'Parsed ->
  Sem r (Statement 'Scoped)
checkStatement s = topBindings $ case s of
  StatementSyntax synDef -> StatementSyntax <$> checkSyntaxDef synDef
  StatementTypeSignature tySig -> StatementTypeSignature <$> checkTypeSignature tySig
  StatementImport imp -> StatementImport <$> checkImport imp
  StatementInductive dt -> StatementInductive <$> checkInductiveDef dt
  StatementModule dt -> StatementModule <$> checkLocalModule dt
  StatementOpenModule open -> StatementOpenModule <$> checkOpenModule open
  StatementFunctionClause clause -> StatementFunctionClause <$> checkFunctionClause clause
  StatementAxiom ax -> StatementAxiom <$> checkAxiomDef ax

checkSyntaxDef ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities] r) =>
  SyntaxDef ->
  Sem r SyntaxDef
checkSyntaxDef = \case
  SyntaxOperator opDef -> SyntaxOperator opDef <$ checkOperatorSyntaxDef opDef

-------------------------------------------------------------------------------
-- Infix Expression
-------------------------------------------------------------------------------

makeExpressionTable ::
  ExpressionAtoms 'Scoped -> [[P.Operator Parse Expression]]
makeExpressionTable (ExpressionAtoms atoms _) = [appOpExplicit] : operators ++ [[functionOp]]
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
          | Just Fixity {..} <- _nameFixity = Just $
              case _fixityArity of
                Unary u -> (_fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                  where
                    unaryApp :: ScopedIden -> Expression -> Expression
                    unaryApp funName arg = case u of
                      AssocPostfix -> ExpressionPostfixApplication (PostfixApplication arg funName)
                Binary b -> (_fixityPrecedence, infixLRN (binaryApp <$> parseSymbolId _nameId))
                  where
                    binaryApp :: ScopedIden -> Expression -> Expression -> Expression
                    binaryApp _infixAppOperator _infixAppLeft _infixAppRight =
                      ExpressionInfixApplication InfixApplication {..}
                    infixLRN :: Parse (Expression -> Expression -> Expression) -> P.Operator Parse Expression
                    infixLRN = case b of
                      AssocLeft -> P.InfixL
                      AssocRight -> P.InfixR
                      AssocNone -> P.InfixN
          | otherwise = Nothing
          where
            S.Name' {..} = identifierName iden
        parseSymbolId :: S.NameId -> Parse ScopedIden
        parseSymbolId uid = P.token getIdentifierWithId mempty
          where
            getIdentifierWithId :: ExpressionAtom 'Scoped -> Maybe ScopedIden
            getIdentifierWithId s = case s of
              AtomIdentifier iden
                | uid == identifierName iden ^. S.nameId -> Just iden
              _ -> Nothing

    -- Application by juxtaposition.
    appOpExplicit :: P.Operator Parse Expression
    appOpExplicit = P.InfixL (return app)
      where
        app :: Expression -> Expression -> Expression
        app f x =
          ExpressionApplication
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
        nonDepFun _funKw a b =
          ExpressionFunction
            Function
              { _funParameters = param,
                _funReturn = b,
                _funKw
              }
          where
            param =
              FunctionParameters
                { _paramNames = [],
                  _paramImplicit = Explicit,
                  _paramType = a
                }

parseExpressionAtoms ::
  forall r.
  (Members '[Error ScoperError, State Scope] r) =>
  ExpressionAtoms 'Scoped ->
  Sem r Expression
parseExpressionAtoms a@(ExpressionAtoms sections _) = do
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
    res = P.parse parser filePath (toList sections)
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
      <|> parseFunction
      <|> parseLambda
      <|> parseCase
      <|> parseLiteral
      <|> parseLetBlock
      <|> parseBraces
  where
    parseHole :: Parse Expression
    parseHole = ExpressionHole <$> P.token lit mempty
      where
        lit :: ExpressionAtom 'Scoped -> Maybe Hole
        lit s = case s of
          AtomHole l -> Just l
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

    parseLetBlock :: Parse Expression
    parseLetBlock = ExpressionLetBlock <$> P.token letBlock mempty
      where
        letBlock :: ExpressionAtom 'Scoped -> Maybe (LetBlock 'Scoped)
        letBlock s = case s of
          AtomLetBlock u -> Just u
          _ -> Nothing

    parseNoInfixIdentifier :: Parse Expression
    parseNoInfixIdentifier = ExpressionIdentifier <$> P.token identifierNoFixity mempty
      where
        identifierNoFixity :: ExpressionAtom 'Scoped -> Maybe ScopedIden
        identifierNoFixity s = case s of
          AtomIdentifier iden
            | not (S.hasFixity (identifierName iden)) -> Just iden
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
    getConstructorRef :: PatternAtom 'Scoped -> Maybe ConstructorRef
    getConstructorRef = \case
      PatternAtomIden i -> case i of
        PatternScopedConstructor c -> Just c
        _ -> Nothing
      _ -> Nothing
    operators = mkSymbolTable (mapMaybe getConstructorRef (toList latoms))
    mkSymbolTable :: [ConstructorRef] -> [[P.Operator ParsePat PatternArg]]
    mkSymbolTable = reverse . map (map snd) . groupSortOn' fst . mapMaybe unqualifiedSymbolOp
      where
        unqualifiedSymbolOp :: ConstructorRef -> Maybe (Precedence, P.Operator ParsePat PatternArg)
        unqualifiedSymbolOp (ConstructorRef' S.Name' {..})
          | Just Fixity {..} <- _nameFixity,
            _nameKind == S.KNameConstructor = Just $
              case _fixityArity of
                Unary u -> (_fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                  where
                    unaryApp :: ConstructorRef -> PatternArg -> PatternArg
                    unaryApp funName = case u of
                      AssocPostfix -> explicitP . PatternPostfixApplication . (`PatternPostfixApp` funName)
                Binary b -> (_fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId _nameId))
                  where
                    binaryInfixApp :: ConstructorRef -> PatternArg -> PatternArg -> PatternArg
                    binaryInfixApp name argLeft = explicitP . PatternInfixApplication . PatternInfixApp argLeft name
                    infixLRN :: ParsePat (PatternArg -> PatternArg -> PatternArg) -> P.Operator ParsePat PatternArg
                    infixLRN = case b of
                      AssocLeft -> P.InfixL
                      AssocRight -> P.InfixR
                      AssocNone -> P.InfixN
          | otherwise = Nothing
        parseSymbolId :: S.NameId -> ParsePat ConstructorRef
        parseSymbolId uid = P.token getConstructorRefWithId mempty
          where
            getConstructorRefWithId :: PatternAtom 'Scoped -> Maybe ConstructorRef
            getConstructorRefWithId s = do
              ref <- getConstructorRef s
              guard (ref ^. constructorRefName . S.nameId == uid)
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
explicitP = PatternArg Explicit Nothing

implicitP :: Pattern -> PatternArg
implicitP = PatternArg Implicit Nothing

parsePatternTerm ::
  forall r.
  (Members '[Embed ParsePat] r) =>
  Sem r PatternArg
parsePatternTerm = do
  embed @ParsePat $
    parseNoInfixConstructor
      <|> parseVariable
      <|> parseParens
      <|> parseBraces
      <|> parseWildcard
      <|> parseEmpty
      <|> parseAt
  where
    parseNoInfixConstructor :: ParsePat PatternArg
    parseNoInfixConstructor =
      explicitP . PatternConstructor
        <$> P.token constructorNoFixity mempty
      where
        constructorNoFixity :: PatternAtom 'Scoped -> Maybe ConstructorRef
        constructorNoFixity s = case s of
          PatternAtomIden (PatternScopedConstructor ref)
            | not (S.hasFixity n) -> Just ref
            where
              n = ref ^. constructorRefName
          _ -> Nothing

    parseWildcard :: ParsePat PatternArg
    parseWildcard = explicitP . PatternWildcard <$> P.token isWildcard mempty
      where
        isWildcard :: PatternAtom 'Scoped -> Maybe Wildcard
        isWildcard s = case s of
          PatternAtomWildcard i -> Just i
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
                Just (Left (ErrDoubleBracesPattern (DoubleBracesPattern r)))
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
  (Members '[Error ScoperError, State Scope] r) =>
  PatternAtom 'Scoped ->
  Sem r PatternArg
parsePatternAtom = parsePatternAtoms . singletonAtom
  where
    singletonAtom :: PatternAtom 'Scoped -> PatternAtoms 'Scoped
    singletonAtom a = PatternAtoms (NonEmpty.singleton a) (getLoc a)

parsePatternAtoms ::
  (Members '[Error ScoperError, State Scope] r) =>
  PatternAtoms 'Scoped ->
  Sem r PatternArg
parsePatternAtoms atoms@(PatternAtoms sec' _) = do
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
