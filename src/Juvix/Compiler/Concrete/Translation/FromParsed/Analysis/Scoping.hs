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
import Juvix.Compiler.Concrete.Data.InfoTableBuilder
import Juvix.Compiler.Concrete.Data.Name qualified as N
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.Scope qualified as S
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
  (Members '[Error ScoperError, NameIdGen, Reader EntryPoint] r) =>
  ParserResult ->
  NonEmpty (Module 'Parsed 'ModuleTop) ->
  Sem r ScoperResult
scopeCheck pr modules =
  fmap mkResult $
    runInfoTableBuilder emptyInfoTable $
      runReader iniScopeParameters $
        runState iniScoperState $
          checkTopModules modules
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
          _resultScope = scoperSt ^. scoperScope
        }

scopeCheckExpression ::
  forall r.
  (Members '[Error JuvixError, NameIdGen] r) =>
  InfoTable ->
  S.Scope ->
  ExpressionAtoms 'Parsed ->
  Sem r Expression
scopeCheckExpression tab scope as = mapError (JuvixError @ScoperError) $ do
  snd
    <$> runInfoTableBuilder
      tab
      ( runReader iniScopeParameters $
          evalState iniScoperState $
            evalState scope $
              localScope $
                checkParseExpressionAtoms as
      )
  where
    iniScopeParameters :: ScopeParameters
    iniScopeParameters =
      ScopeParameters
        { _scopeTopParents = mempty,
          _scopeParsedModules = mempty
        }

checkParseExpressionAtoms' ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms' = checkExpressionAtoms >=> parseExpressionAtoms

freshVariable :: (Members '[NameIdGen, State Scope] r) => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol S.KNameLocal

freshSymbol ::
  forall r.
  (Members '[State Scope, NameIdGen] r) =>
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
      | S.canHaveFixity _nameKind =
          fmap (^. opFixity) . HashMap.lookup _nameConcrete <$> gets (^. scopeFixities)
      | otherwise = return Nothing

reserveSymbolOf ::
  forall r.
  (Members '[Error ScoperError, NameIdGen, State Scope] r) =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolOf k s = do
  checkNotBound
  freshSymbol k s
  where
    checkNotBound :: Sem r ()
    checkNotBound = do
      path <- gets (^. scopePath)
      syms <- gets (^. scopeSymbols)
      let exists = HashMap.lookup s syms >>= HashMap.lookup path . (^. symbolInfo)
      whenJust exists $
        \e ->
          throw
            ( ErrMultipleDeclarations
                MultipleDeclarations
                  { _multipleDeclEntry = e,
                    _multipleDeclSymbol = s ^. symbolText,
                    _multipleDeclSecond = getLoc s
                  }
            )

bindReservedSymbol ::
  (Members '[State Scope] r) =>
  S.Symbol ->
  SymbolEntry ->
  Sem r ()
bindReservedSymbol s' entry = do
  path <- gets (^. scopePath)
  modify (over scopeSymbols (HashMap.alter (Just . addS path) s))
  where
    s = s' ^. S.nameConcrete
    addS :: S.AbsModulePath -> Maybe SymbolInfo -> SymbolInfo
    addS path m = case m of
      Nothing -> symbolInfoSingle entry
      Just SymbolInfo {..} -> SymbolInfo (HashMap.insert path entry _symbolInfo)

bindSymbolOf ::
  (Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder] r) =>
  S.NameKind ->
  (S.Name' () -> SymbolEntry) ->
  Symbol ->
  Sem r S.Symbol
bindSymbolOf k e s = do
  s' <- reserveSymbolOf k s
  bindReservedSymbol s' (e (set S.nameConcrete () s'))
  registerName (S.unqualifiedSymbol s')
  return s'

bindFunctionSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder] r) =>
  Symbol ->
  Sem r S.Symbol
bindFunctionSymbol = bindSymbolOf S.KNameFunction (EntryFunction . FunctionRef')

bindInductiveSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder] r) =>
  Symbol ->
  Sem r S.Symbol
bindInductiveSymbol =
  bindSymbolOf
    S.KNameInductive
    (EntryInductive . InductiveRef')

bindAxiomSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder] r) =>
  Symbol ->
  Sem r S.Symbol
bindAxiomSymbol =
  bindSymbolOf
    S.KNameAxiom
    (EntryAxiom . AxiomRef')

bindConstructorSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder] r) =>
  Symbol ->
  Sem r S.Symbol
bindConstructorSymbol =
  bindSymbolOf
    S.KNameConstructor
    (EntryConstructor . ConstructorRef')

bindLocalModuleSymbol ::
  (Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder] r) =>
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
checkImport import_@(Import kw path) = do
  checkCycle
  cache <- gets (^. scoperModulesCache . cachedModules)
  moduleRef <- maybe (readScopeModule import_) return (cache ^. at path)
  let checked = moduleRef ^. moduleRefModule
      sname :: S.TopModulePath = checked ^. modulePath
      sname' :: S.Name = set S.nameConcrete (topModulePathToName path) sname
      moduleId = sname ^. S.nameId
      cmoduleRef :: ModuleRef'' 'S.Concrete 'ModuleTop = set moduleRefName sname' moduleRef
  modify (over scopeTopModules (HashMap.insert path moduleRef))
  registerName (set S.nameConcrete path sname)
  let moduleRef' = mkModuleRef' moduleRef
  modify (over scoperModules (HashMap.insert moduleId moduleRef'))
  return (Import kw cmoduleRef)
  where
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
  (Members '[State Scope, Error ScoperError] r) =>
  [Symbol] ->
  Symbol ->
  Sem r [SymbolEntry]
lookupSymbolAux modules final = do
  local' <- hereOrInLocalModule
  import' <- importedTopModule
  return $ local' ++ maybeToList import'
  where
    hereOrInLocalModule :: Sem r [SymbolEntry] =
      case modules of
        [] -> do
          r <- HashMap.lookup final <$> gets (^. scopeSymbols)
          return $ case r of
            Nothing -> []
            Just SymbolInfo {..} -> toList _symbolInfo
        (p : ps) ->
          mapMaybe (lookInExport final ps . getModuleExportInfo)
            . concat
            . maybeToList
            . fmap (mapMaybe getModuleRef . toList . (^. symbolInfo))
            . HashMap.lookup p
            <$> gets (^. scopeSymbols)
    importedTopModule :: Sem r (Maybe SymbolEntry)
    importedTopModule = do
      fmap (EntryModule . mkModuleRef') . HashMap.lookup path <$> gets (^. scopeTopModules)
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
  (Members '[State Scope, Error ScoperError, State ScoperState] r) =>
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
      concatMapM (fmap maybeToList . uncurry lookInTopModule) allTopPaths
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
        lookInTopModule :: TopModulePath -> [Symbol] -> Sem r (Maybe SymbolEntry)
        lookInTopModule topPath remaining =
          ((fmap (^. moduleExportInfo) . HashMap.lookup topPath) >=> lookInExport sym remaining) <$> gets (^. scopeTopModules)

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
  (Members '[Error ScoperError, State Scope] r) =>
  OperatorSyntaxDef ->
  Sem r ()
checkOperatorSyntaxDef s@OperatorSyntaxDef {..} = do
  checkNotDefined
  modify (over scopeFixities (HashMap.insert _opSymbol s))
  where
    checkNotDefined :: Sem r ()
    checkNotDefined =
      whenJustM
        (HashMap.lookup _opSymbol <$> gets (^. scopeFixities))
        (\s' -> throw (ErrDuplicateFixity (DuplicateFixity s' s)))

checkTypeSignature ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature TypeSignature {..} = do
  sigType' <- checkParseExpressionAtoms _sigType
  sigName' <- bindFunctionSymbol _sigName
  sigDoc' <- mapM checkJudoc _sigDoc
  sigBody' <- mapM checkParseExpressionAtoms _sigBody
  registerFunction' TypeSignature {_sigName = sigName', _sigType = sigType', _sigDoc = sigDoc', _sigBody = sigBody', ..}

checkConstructorDef ::
  (Members '[Error ScoperError, Reader LocalVars, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  InductiveConstructorDef 'Parsed ->
  Sem r (InductiveConstructorDef 'Scoped)
checkConstructorDef InductiveConstructorDef {..} = do
  constructorType' <- checkParseExpressionAtoms _constructorType
  constructorName' <- bindConstructorSymbol _constructorName
  doc' <- mapM checkJudoc _constructorDoc
  registerConstructor'
    InductiveConstructorDef
      { _constructorName = constructorName',
        _constructorType = constructorType',
        _constructorDoc = doc',
        _constructorPipe
      }

withParams ::
  forall r a.
  (Members '[Reader LocalVars, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  [InductiveParameters 'Parsed] ->
  ([InductiveParameters 'Scoped] -> Sem r a) ->
  Sem r a
withParams xs a = go [] [] xs
  where
    go :: [InductiveParameters 'Scoped] -> [Symbol] -> [InductiveParameters 'Parsed] -> Sem r a
    go inductiveParameters' usedNames params =
      case params of
        -- All params have been checked
        [] -> a inductiveParameters'
        -- More params to check
        (InductiveParameters {..} : ps) ->
          go' [] usedNames (toList _inductiveParametersNames)
          where
            go' :: [SymbolType 'Scoped] -> [Symbol] -> [SymbolType 'Parsed] -> Sem r a
            go' names' usedNames' = \case
              [] -> do
                inductiveParametersType' <- checkParseExpressionAtoms _inductiveParametersType
                let param' =
                      InductiveParameters
                        { _inductiveParametersType = inductiveParametersType',
                          _inductiveParametersNames = NonEmpty.fromList (reverse names')
                        }
                go (inductiveParameters' ++ [param']) usedNames' ps
              nm : names
                | nm `elem` usedNames' ->
                    throw
                      ( ErrDuplicateInductiveParameterName
                          (DuplicateInductiveParameterName nm)
                      )
                | otherwise -> do
                    nm' <- freshVariable nm
                    withBindLocalVariable (LocalVariable nm') $
                      go' (nm' : names') (nm : usedNames) names

checkInductiveDef ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  InductiveDef 'Parsed ->
  Sem r (InductiveDef 'Scoped)
checkInductiveDef ty@InductiveDef {..} = do
  withParams _inductiveParameters $ \inductiveParameters' -> do
    inductiveType' <- mapM checkParseExpressionAtoms _inductiveType
    inductiveDoc' <- mapM checkJudoc _inductiveDoc
    inductiveName' <- bindInductiveSymbol _inductiveName
    inductiveConstructors' <- mapM checkConstructorDef _inductiveConstructors
    registerInductive'
      InductiveDef
        { _inductiveName = inductiveName',
          _inductiveDoc = inductiveDoc',
          _inductiveBuiltin = _inductiveBuiltin,
          _inductiveParameters = inductiveParameters',
          _inductiveType = inductiveType',
          _inductiveConstructors = inductiveConstructors',
          _inductivePositive = ty ^. inductivePositive,
          _inductiveKw
        }

createExportsTable :: ExportInfo -> HashSet NameId
createExportsTable ei = foldr (HashSet.insert . getNameId) HashSet.empty (HashMap.elems (ei ^. exportSymbols))
  where
    getNameId = \case
      EntryAxiom r -> getNameRefId (r ^. axiomRefName)
      EntryInductive r -> getNameRefId (r ^. inductiveRefName)
      EntryFunction r -> getNameRefId (r ^. functionRefName)
      EntryConstructor r -> getNameRefId (r ^. constructorRefName)
      EntryModule r -> getModuleRefNameId r

checkTopModules ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  NonEmpty (Module 'Parsed 'ModuleTop) ->
  Sem r (NonEmpty (Module 'Scoped 'ModuleTop), HashSet NameId)
checkTopModules modules = do
  r <- checkTopModule (head modules)
  mods <- (r ^. moduleRefModule :|) <$> mapM checkTopModule_ (NonEmpty.tail modules)
  return (mods, createExportsTable (r ^. moduleExportInfo))

checkTopModule_ ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule_ = fmap (^. moduleRefModule) . checkTopModule

checkTopModule ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Module 'Parsed 'ModuleTop ->
  Sem r (ModuleRef'' 'S.NotConcrete 'ModuleTop)
checkTopModule m@(Module _moduleKw path params doc body) = do
  r <- checkedModule
  modify (over (scoperModulesCache . cachedModules) (HashMap.insert path r))
  registerModule (r ^. moduleRefModule)
  return r
  where
    freshTopModulePath ::
      forall s.
      (Members '[State ScoperState, NameIdGen] s) =>
      Sem s S.TopModulePath
    freshTopModulePath = do
      _nameId <- freshNameId
      let _nameDefinedIn = S.topModulePathToAbsPath path
          _nameConcrete = path
          _nameDefined = getLoc (path ^. modulePathName)
          _nameKind = S.KNameTopModule
          _nameFixity :: Maybe Fixity
          _nameFixity = Nothing
          -- This visibility annotation is not relevant
          _nameVisibilityAnn = VisPublic
          _nameWhyInScope = S.BecauseDefined
          _nameVerbatim = N.topModulePathToDottedPath path
          moduleName = S.Name' {..}
      return moduleName
    iniScope :: Scope
    iniScope = emptyScope (getTopModulePath m)
    checkedModule :: Sem r (ModuleRef'' 'S.NotConcrete 'ModuleTop)
    checkedModule = do
      (s, (m', p)) <- runState iniScope $ do
        path' <- freshTopModulePath
        localScope $
          withParams params $ \params' -> do
            (_moduleExportInfo, body') <- checkModuleBody body
            doc' <- mapM checkJudoc doc
            let _moduleRefModule =
                  Module
                    { _modulePath = path',
                      _moduleParameters = params',
                      _moduleBody = body',
                      _moduleDoc = doc',
                      _moduleKw
                    }
                _moduleRefName = set S.nameConcrete () path'
            return (ModuleRef'' {..}, path')
      modify (set (scoperScope . at (p ^. S.nameConcrete)) (Just s))
      return m'

withScope :: (Members '[State Scope] r) => Sem r a -> Sem r a
withScope ma = do
  before <- get @Scope
  x <- ma
  put before
  return x

checkModuleBody ::
  forall r.
  (Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  [Statement 'Parsed] ->
  Sem r (ExportInfo, [Statement 'Scoped])
checkModuleBody body = do
  body' <- mapM checkStatement body
  checkOrphanFixities
  exported <- get >>= exportScope
  return (exported, body')

checkLocalModule ::
  forall r.
  (Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule Module {..} = do
  (_moduleExportInfo, moduleBody', moduleParameters', moduleDoc') <-
    withScope $
      withParams _moduleParameters $ \p' -> do
        inheritScope
        (e, b) <- checkModuleBody _moduleBody
        doc' <- mapM checkJudoc _moduleDoc
        return (e, b, p', doc')
  _modulePath' <- reserveSymbolOf S.KNameLocalModule _modulePath
  let moduleId = _modulePath' ^. S.nameId
      _moduleRefName = set S.nameConcrete () _modulePath'
      _moduleRefModule =
        Module
          { _modulePath = _modulePath',
            _moduleParameters = moduleParameters',
            _moduleBody = moduleBody',
            _moduleDoc = moduleDoc',
            _moduleKw
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
      modify (set scopeFixities mempty) -- do not inherit fixity declarations
      where
        inheritSymbol :: SymbolInfo -> SymbolInfo
        inheritSymbol (SymbolInfo s) = SymbolInfo (fmap inheritEntry s)
        inheritEntry :: SymbolEntry -> SymbolEntry
        inheritEntry = entryOverName (over S.nameWhyInScope S.BecauseInherited . set S.nameVisibilityAnn VisPrivate)

checkOrphanFixities :: forall r. (Members '[Error ScoperError, State Scope] r) => Sem r ()
checkOrphanFixities = do
  path <- gets (^. scopePath)
  declared <- gets (^. scopeFixities)
  used <- gets (HashMap.keys . fmap (filter (== path) . HashMap.keys . (^. symbolInfo)) . (^. scopeSymbols))
  let unused = toList $ foldr HashMap.delete declared used
  case unused of
    [] -> return ()
    (x : _) -> throw (ErrUnusedOperatorDef (UnusedOperatorDef x))

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
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenImportModule op
  | Just k <- op ^. openModuleImportKw =
      let import_ :: Import 'Parsed
          import_ = Import k (moduleNameToTopModulePath (op ^. openModuleName))
       in do
            void (checkImport import_)
            scopedOpen <- checkOpenModule (set openModuleImportKw Nothing op)
            return (set openModuleImportKw (Just k) scopedOpen)
  | otherwise = impossible

checkOpenModuleNoImport ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenModuleNoImport OpenModule {..}
  | isJust _openModuleImportKw = error "unsupported: open import statement"
  | otherwise = do
      openModuleName'@(ModuleRef' (_ :&: moduleRef'')) <- lookupModuleSymbol _openModuleName
      openParameters' <- mapM checkParseExpressionAtoms _openParameters
      mergeScope (alterScope (moduleRef'' ^. moduleExportInfo))
      registerName (moduleRef'' ^. moduleRefName)
      return
        OpenModule
          { _openModuleName = openModuleName',
            _openParameters = openParameters',
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
    setsUsingHiding :: Maybe (Either (HashSet Symbol) (HashSet Symbol))
    setsUsingHiding = case _openUsingHiding of
      Just (Using l) -> Just (Left (HashSet.fromList (toList l)))
      Just (Hiding l) -> Just (Right (HashSet.fromList (toList l)))
      Nothing -> Nothing
    alterScope :: ExportInfo -> ExportInfo
    alterScope = alterEntries . filterScope
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
        filterScope = over exportSymbols filterTable
          where
            filterTable :: HashMap Symbol a -> HashMap Symbol a
            filterTable = HashMap.filterWithKey (const . shouldOpen)
        shouldOpen :: Symbol -> Bool
        shouldOpen s = case setsUsingHiding of
          Nothing -> True
          Just (Left using) -> HashSet.member s using
          Just (Right hiding) -> not (HashSet.member s hiding)

checkOpenModule ::
  forall r.
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenModule op
  | isJust (op ^. openModuleImportKw) = checkOpenImportModule op
  | otherwise = checkOpenModuleNoImport op

checkFunctionClause ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  FunctionClause 'Parsed ->
  Sem r (FunctionClause 'Scoped)
checkFunctionClause clause@FunctionClause {..} = do
  clauseOwnerFunction' <- checkTypeSigInScope
  registerName (S.unqualifiedSymbol clauseOwnerFunction')
  (clausePatterns', clauseBody') <- do
    clp <- mapM checkParsePatternAtom _clausePatterns
    withBindCurrentGroup $ do
      s <- get @Scope
      clb <- checkParseExpressionAtoms _clauseBody
      put s
      return (clp, clb)
  registerFunctionClause'
    FunctionClause
      { _clauseOwnerFunction = clauseOwnerFunction',
        _clausePatterns = clausePatterns',
        _clauseBody = clauseBody'
      }
  where
    fun = _clauseOwnerFunction
    checkTypeSigInScope :: Sem r S.Symbol
    checkTypeSigInScope = do
      e <- fromMaybeM err (lookupLocalEntry fun)
      case e of
        EntryFunction ref -> return (set S.nameConcrete fun (ref ^. functionRefName))
        _ -> err
      where
        err :: Sem r a
        err = throw (ErrLacksTypeSig (LacksTypeSig clause))

lookupLocalEntry ::
  (Members '[Error ScoperError, State Scope, State ScoperState] r) =>
  Symbol ->
  Sem r (Maybe SymbolEntry)
lookupLocalEntry sym = do
  ms <- HashMap.lookup sym <$> gets (^. scopeSymbols)
  path <- gets (^. scopePath)
  -- The symbol must be defined in the same path
  return $ do
    SymbolInfo {..} <- ms
    HashMap.lookup path _symbolInfo

localScope :: forall r a. Sem (Reader LocalVars : r) a -> Sem r a
localScope = runReader (LocalVars mempty)

checkAxiomDef ::
  (Members '[InfoTableBuilder, Error ScoperError, State Scope, State ScoperState, NameIdGen] r) =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiomDef AxiomDef {..} = do
  axiomType' <- localScope (checkParseExpressionAtoms _axiomType)
  axiomName' <- bindAxiomSymbol _axiomName
  axiomDoc' <- localScope (mapM checkJudoc _axiomDoc)
  registerAxiom' AxiomDef {_axiomName = axiomName', _axiomType = axiomType', _axiomDoc = axiomDoc', ..}

checkCompile ::
  (Members '[InfoTableBuilder, Error ScoperError, State Scope, Reader LocalVars, State ScoperState] r) =>
  Compile 'Parsed ->
  Sem r (Compile 'Scoped)
checkCompile c@Compile {..} = do
  scopedSym :: S.Symbol <- checkCompileName c
  let sym :: Symbol = c ^. compileName
  rules <- gets (^. scopeCompilationRules)
  if
      | Just info <- HashMap.lookup sym rules ->
          throw
            ( ErrMultipleCompileBlockSameName
                ( MultipleCompileBlockSameName
                    { _multipleCompileBlockFirstDefined = info ^. compileInfoDefined,
                      _multipleCompileBlockSym = sym
                    }
                )
            )
      | otherwise -> do
          void (checkBackendItems sym _compileBackendItems mempty)
          registerName (S.unqualifiedSymbol scopedSym)
          modify
            ( over
                scopeCompilationRules
                ( HashMap.insert
                    _compileName
                    ( CompileInfo
                        { _compileInfoBackendItems = _compileBackendItems,
                          _compileInfoDefined = getLoc scopedSym
                        }
                    )
                )
            )
          registerCompile' $ Compile {_compileName = scopedSym, ..}

checkBackendItems ::
  (Members '[Error ScoperError] r) =>
  Symbol ->
  [BackendItem] ->
  HashSet Backend ->
  Sem r (HashSet Backend)
checkBackendItems _ [] bset = return bset
checkBackendItems sym (b : bs) bset =
  let cBackend = b ^. backendItemBackend . withLocParam
   in if
          | HashSet.member cBackend bset ->
              throw
                ( ErrMultipleCompileRuleSameBackend
                    (MultipleCompileRuleSameBackend b sym)
                )
          | otherwise -> checkBackendItems sym bs (HashSet.insert cBackend bset)

checkCompileName ::
  (Members '[Error ScoperError, State Scope, Reader LocalVars, State ScoperState, InfoTableBuilder] r) =>
  Compile 'Parsed ->
  Sem r S.Symbol
checkCompileName Compile {..} = do
  let sym :: Symbol = _compileName
      name :: Name = NameUnqualified sym
  scope <- get
  locals <- ask
  entries <- lookupQualifiedSymbol ([], sym)
  case filter S.canBeCompiled entries of
    [] -> case entries of
      [] -> throw (ErrSymNotInScope (NotInScope sym locals scope))
      (e : _) ->
        throw
          ( ErrWrongKindExpressionCompileBlock
              ( WrongKindExpressionCompileBlock
                  { _wrongKindExpressionCompileBlockSym = sym,
                    _wrongKindExpressionCompileBlockEntry = e
                  }
              )
          )
    [x] -> do
      actualPath <- gets (^. scopePath)
      let scoped = entryToSymbol x sym
          expectedPath = scoped ^. S.nameDefinedIn
      if
          | actualPath == expectedPath -> return scoped
          | otherwise ->
              throw
                ( ErrWrongLocationCompileBlock
                    (WrongLocationCompileBlock expectedPath name)
                )
    xs -> throw (ErrAmbiguousSym (AmbiguousSym name xs))

entryToSymbol :: SymbolEntry -> Symbol -> S.Symbol
entryToSymbol sentry csym = set S.nameConcrete csym (symbolEntryToSName sentry)

checkEval ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Eval 'Parsed ->
  Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> localScope (checkParseExpressionAtoms s)

checkPrint ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  Print 'Parsed ->
  Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> localScope (checkParseExpressionAtoms s)

checkFunction ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction Function {..} = do
  funParameters' <- checkParams
  let scoped = foldr go id (funParameters' ^. paramNames)
  funReturn' <- scoped (checkParseExpressionAtoms _funReturn)
  return
    Function
      { _funParameters = funParameters',
        _funReturn = funReturn'
      }
  where
    go :: Maybe (SymbolType 'Scoped) -> (Sem r a -> Sem r a) -> (Sem r a -> Sem r a)
    go param acc = case param of
      Nothing -> acc
      Just s -> withBindLocalVariable (LocalVariable s) . acc
    checkParams :: Sem r (FunctionParameters 'Scoped)
    checkParams = do
      paramType' <- checkParseExpressionAtoms _paramType
      paramNames' <- checkParamNames
      return
        FunctionParameters
          { _paramNames = paramNames',
            _paramImplicit = _paramImplicit,
            _paramType = paramType'
          }
      where
        FunctionParameters {..} = _funParameters
        checkParamNames :: Sem r (NonEmpty (Maybe S.Symbol))
        checkParamNames =
          forM
            _paramNames
            ( \case
                Nothing -> return Nothing
                Just s -> Just <$> freshVariable s
            )

checkLetClause ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  LetClause 'Parsed ->
  Sem r (LetClause 'Scoped)
checkLetClause lc = case lc of
  LetTypeSig t -> LetTypeSig <$> checkTypeSignature t
  LetFunClause c -> LetFunClause <$> checkFunctionClause c

checkLetBlock ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  LetBlock 'Parsed ->
  Sem r (LetBlock 'Scoped)
checkLetBlock LetBlock {..} = do
  s <- get @Scope -- backup scope: we do not want local definitions to stay in scope
  letClauses' <- mapM checkLetClause _letClauses
  letExpression' <- checkParseExpressionAtoms _letExpression
  put s -- restore scope
  return
    LetBlock
      { _letClauses = letClauses',
        _letExpression = letExpression',
        _letKw
      }

checkCaseBranch ::
  forall r.
  Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  CaseBranch 'Parsed ->
  Sem r (CaseBranch 'Scoped)
checkCaseBranch CaseBranch {..} = do
  pattern' <- checkParsePatternAtoms _caseBranchPattern
  checkNotImplicit pattern'
  expression' <- withBindCurrentGroup (checkParseExpressionAtoms _caseBranchExpression)
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
  Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
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
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda Lambda {..} = Lambda _lambdaKw <$> mapM checkLambdaClause _lambdaClauses

checkLambdaClause ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  LambdaClause 'Parsed ->
  Sem r (LambdaClause 'Scoped)
checkLambdaClause LambdaClause {..} = do
  lambdaParameters' <- mapM checkParsePatternAtom _lambdaParameters
  lambdaBody' <- withBindCurrentGroup (checkParseExpressionAtoms _lambdaBody)
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
scopedVar (LocalVariable s) n = do
  let scoped = set S.nameConcrete n s
  registerName (S.unqualifiedSymbol scoped)
  return scoped

checkUnqualified ::
  (Members '[Error ScoperError, State Scope, Reader LocalVars, State ScoperState, InfoTableBuilder] r) =>
  Symbol ->
  Sem r ScopedIden
checkUnqualified s = do
  -- Local vars have scope priority
  l <- HashMap.lookup s <$> asks (^. localVars)
  case l of
    Just v -> ScopedVar <$> scopedVar v s
    Nothing -> do
      scope <- get
      locals <- ask
      -- Lookup at the global scope
      let err = throw (ErrSymNotInScope (NotInScope s locals scope))
      entries <-
        filter S.isExprKind
          <$> lookupQualifiedSymbol ([], s)
      case entries of
        [] -> err
        [x] -> entryToScopedIden n x
        es -> throw (ErrAmbiguousSym (AmbiguousSym n es))
      where
        n = NameUnqualified s

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
    Nothing -> PatternScopedVar <$> groupBindLocalVariable sym -- the symbol is a variable
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
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternBinding ->
  Sem r PatternArg
checkPatternBinding (PatternBinding n p) = do
  n' <- groupBindLocalVariable n
  p' <- checkParsePatternAtom p
  if isJust (p' ^. patternArgName)
    then throw (ErrDoubleBinderPattern (DoubleBinderPattern n' p'))
    else return $ set patternArgName (Just n') p'

withBindCurrentGroup ::
  (Members '[State Scope, Reader LocalVars] r) =>
  Sem r a ->
  Sem r a
withBindCurrentGroup ma = do
  grp <- gets (^. scopeBindGroup)
  modify (over scopeBindGroup (const mempty)) -- empties the group
  local (over localVars (HashMap.union grp)) ma

addLocalVars :: [LocalVariable] -> LocalVars -> LocalVars
addLocalVars lv = over localVars (flip (foldr insertVar) lv)
  where
    insertVar v = HashMap.insert (v ^. variableName . S.nameConcrete) v

withBindLocalVariable ::
  (Members '[Reader LocalVars] r) =>
  LocalVariable ->
  Sem r a ->
  Sem r a
withBindLocalVariable var = local (addLocalVars [var])

-- | Binds a local variable in a bind group, i.e. a group of pattern.
groupBindLocalVariable ::
  forall r.
  (Members '[Error ScoperError, State Scope, NameIdGen] r) =>
  Symbol ->
  Sem r S.Symbol
groupBindLocalVariable s = do
  checkNotInGroup
  addToGroup
  where
    checkNotInGroup :: Sem r ()
    checkNotInGroup =
      whenJustM
        (HashMap.lookup s <$> gets (^. scopeBindGroup))
        ( \x ->
            throw
              ( ErrBindGroup
                  BindGroupConflict
                    { _bindGroupFirst = x ^. variableName . S.nameConcrete,
                      _bindGroupSecond = s
                    }
              )
        )
    addToGroup :: Sem r S.Symbol
    addToGroup = do
      n <- freshVariable s
      modify (over scopeBindGroup (HashMap.insert s (LocalVariable n)))
      return n

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
  (Members '[Error ScoperError, State Scope, Reader LocalVars, State ScoperState, InfoTableBuilder] r) =>
  Name ->
  Sem r ScopedIden
checkName n = case n of
  NameQualified q -> checkQualifiedExpr q
  NameUnqualified s -> checkUnqualified s

checkExpressionAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
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
  AtomFunArrow -> return AtomFunArrow
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
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
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
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l i) = do
  (`ExpressionAtoms` i) <$> mapM checkExpressionAtom l

checkJudoc ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  Judoc 'Parsed ->
  Sem r (Judoc 'Scoped)
checkJudoc (Judoc atoms) = Judoc <$> mapM checkJudocBlock atoms

checkJudocBlock ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  JudocBlock 'Parsed ->
  Sem r (JudocBlock 'Scoped)
checkJudocBlock = \case
  JudocParagraph l -> JudocParagraph <$> mapM checkJudocLine l
  JudocExample e -> JudocExample <$> traverseOf exampleExpression checkParseExpressionAtoms e

checkJudocLine ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  JudocParagraphLine 'Parsed ->
  Sem r (JudocParagraphLine 'Scoped)
checkJudocLine (JudocParagraphLine atoms) = JudocParagraphLine <$> mapM (mapM checkJudocAtom) atoms

checkJudocAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  JudocAtom 'Parsed ->
  Sem r (JudocAtom 'Scoped)
checkJudocAtom = \case
  JudocText t -> return (JudocText t)
  JudocExpression e -> JudocExpression <$> checkParseExpressionAtoms e

checkParseExpressionAtoms ::
  forall r.
  (Members '[Error ScoperError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternAtom 'Parsed ->
  Sem r PatternArg
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkStatement ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r) =>
  Statement 'Parsed ->
  Sem r (Statement 'Scoped)
checkStatement s = case s of
  StatementOperator opDef -> StatementOperator opDef <$ checkOperatorSyntaxDef opDef
  StatementTypeSignature tySig -> StatementTypeSignature <$> checkTypeSignature tySig
  StatementImport imp -> StatementImport <$> checkImport imp
  StatementInductive dt -> StatementInductive <$> checkInductiveDef dt
  StatementModule dt -> StatementModule <$> checkLocalModule dt
  StatementOpenModule open -> StatementOpenModule <$> checkOpenModule open
  StatementFunctionClause clause -> StatementFunctionClause <$> checkFunctionClause clause
  StatementAxiom ax -> StatementAxiom <$> checkAxiomDef ax
  StatementForeign d -> return (StatementForeign d)
  StatementCompile c -> StatementCompile <$> checkCompile c

-------------------------------------------------------------------------------
-- Infix Expression
-------------------------------------------------------------------------------

makeExpressionTable2 ::
  ExpressionAtoms 'Scoped -> [[P.Operator Parse Expression]]
makeExpressionTable2 (ExpressionAtoms atoms _) = [appOpExplicit] : operators ++ [[functionOp]]
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
    functionOp = P.InfixR (nonDepFun <$ P.single AtomFunArrow)
      where
        nonDepFun :: Expression -> Expression -> Expression
        nonDepFun a b =
          ExpressionFunction
            Function
              { _funParameters = param,
                _funReturn = b
              }
          where
            param =
              FunctionParameters
                { _paramNames = NonEmpty.singleton Nothing,
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
    tbl = makeExpressionTable2 a
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
