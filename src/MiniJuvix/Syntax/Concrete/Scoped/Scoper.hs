-- | Limitations:
-- 1. A symbol introduced by a type signature can only be used once per Module.
module MiniJuvix.Syntax.Concrete.Scoped.Scoper
  ( module MiniJuvix.Syntax.Concrete.Scoped.Scoper,
    module MiniJuvix.Syntax.Concrete.Scoped.Scoper.ScoperResult,
    module MiniJuvix.Syntax.Concrete.Scoped.Error,
  )
where

import Control.Monad.Combinators.Expr qualified as P
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Lens.Micro.Platform
import MiniJuvix.Internal.NameIdGen
import MiniJuvix.Pipeline.EntryPoint
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Base qualified as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Name qualified as N
import MiniJuvix.Syntax.Concrete.Parser (runModuleParser)
import MiniJuvix.Syntax.Concrete.Parser qualified as Parser
import MiniJuvix.Syntax.Concrete.Parser.InfoTableBuilder (mergeTable)
import MiniJuvix.Syntax.Concrete.Parser.InfoTableBuilder qualified as Parser
import MiniJuvix.Syntax.Concrete.Parser.ParserResult (ParserResult)
import MiniJuvix.Syntax.Concrete.Scoped.Error
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import MiniJuvix.Syntax.Concrete.Scoped.Scoper.InfoTableBuilder
import MiniJuvix.Syntax.Concrete.Scoped.Scoper.ScoperResult

entryScoper ::
  Members '[Error ScopeError, Files, NameIdGen] r =>
  ParserResult ->
  Sem r ScoperResult
entryScoper pr = do
  let root = pr ^. Parser.resultEntry . entryRoot
      modules = pr ^. Parser.resultModules
  scopeCheck pr root modules

scopeCheck ::
  Members '[Files, Error ScopeError, NameIdGen] r =>
  ParserResult ->
  FilePath ->
  NonEmpty (Module 'Parsed 'ModuleTop) ->
  Sem r ScoperResult
scopeCheck pr root modules =
  fmap mkResult $
    Parser.runInfoTableBuilder $
      runInfoTableBuilder $
        runReader scopeParameters $
          evalState iniScoperState $ do
            mergeTable (pr ^. Parser.resultTable)
            mapM checkTopModule_ modules
  where
    mkResult :: (Parser.InfoTable, (InfoTable, NonEmpty (Module 'Scoped 'ModuleTop))) -> ScoperResult
    mkResult (pt, (st, ms)) =
      ScoperResult
        { _resultParserResult = pr,
          _resultParserTable = pt,
          _resultScoperTable = st,
          _resultModules = ms
        }
    iniScoperState :: ScoperState
    iniScoperState =
      ScoperState
        { _scoperModulesCache = ModulesCache mempty,
          _scoperModules = mempty
        }
    scopeParameters :: ScopeParameters
    scopeParameters =
      ScopeParameters
        { _scopeRootPath = root,
          _scopeFileExtension = ".mjuvix",
          _scopeTopParents = mempty
        }

freshVariable :: Members '[NameIdGen, State Scope] r => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol S.KNameLocal

freshSymbol ::
  forall r.
  Members '[State Scope, NameIdGen] r =>
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
  Members '[Error ScopeError, NameIdGen, State Scope] r =>
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
  Members '[State Scope] r =>
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
  Members '[Error ScopeError, NameIdGen, State Scope, InfoTableBuilder] r =>
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
  Members '[Error ScopeError, NameIdGen, State Scope, InfoTableBuilder] r =>
  Symbol ->
  Sem r S.Symbol
bindFunctionSymbol = bindSymbolOf S.KNameFunction (EntryFunction . FunctionRef')

bindInductiveSymbol ::
  Members '[Error ScopeError, NameIdGen, State Scope, InfoTableBuilder] r =>
  Symbol ->
  Sem r S.Symbol
bindInductiveSymbol =
  bindSymbolOf
    S.KNameInductive
    (EntryInductive . InductiveRef')

bindAxiomSymbol ::
  Members '[Error ScopeError, NameIdGen, State Scope, InfoTableBuilder] r =>
  Symbol ->
  Sem r S.Symbol
bindAxiomSymbol =
  bindSymbolOf
    S.KNameAxiom
    (EntryAxiom . AxiomRef')

bindConstructorSymbol ::
  Members '[Error ScopeError, NameIdGen, State Scope, InfoTableBuilder] r =>
  Symbol ->
  Sem r S.Symbol
bindConstructorSymbol =
  bindSymbolOf
    S.KNameConstructor
    (EntryConstructor . ConstructorRef')

bindLocalModuleSymbol ::
  Members '[Error ScopeError, NameIdGen, State Scope, InfoTableBuilder] r =>
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
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, Files, State ScoperState, InfoTableBuilder, Parser.InfoTableBuilder, NameIdGen] r =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImport import_@(Import path) = do
  checkCycle
  cache <- gets (^. scoperModulesCache . cachedModules)
  moduleRef <- maybe (readParseModule path >>= local addImport . checkTopModule) return (cache ^. at path)
  let checked = moduleRef ^. moduleRefModule
      sname = checked ^. modulePath
      moduleId = sname ^. S.nameId
  modify (over scopeTopModules (HashMap.insert path moduleRef))
  let moduleRef' = mkModuleRef' moduleRef
  modify (over scoperModules (HashMap.insert moduleId moduleRef'))
  return (Import checked)
  where
    addImport :: ScopeParameters -> ScopeParameters
    addImport = over scopeTopParents (cons import_)
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
  (Members '[State Scope, Error ScopeError] r) =>
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
          mapMaybe (lookInExport final ps . getModuleExportInfo) . concat . maybeToList . fmap (mapMaybe getModuleRef . toList . (^. symbolInfo))
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
  Members '[State Scope, Error ScopeError, State ScoperState] r =>
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
    there = concatMapM (fmap maybeToList . uncurry lookInTopModule) allTopPaths
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
  Members '[Error ScopeError, State Scope, State ScoperState, InfoTableBuilder] r =>
  QualifiedName ->
  Sem r ScopedIden
checkQualifiedExpr q@(QualifiedName (Path p) sym) = do
  es <- lookupQualifiedSymbol (toList p, sym)
  case es of
    [] -> notInScope
    [e] -> entryToScopedIden q' e
    _ -> throw (ErrAmbiguousSym (AmbiguousSym q' es))
  where
    q' = NameQualified q
    notInScope = throw (ErrQualSymNotInScope q)

entryToScopedIden :: Members '[InfoTableBuilder] r => Name -> SymbolEntry -> Sem r ScopedIden
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
exportScope :: forall r. Members '[State Scope, Error ScopeError] r => Scope -> Sem r ExportInfo
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

readParseModule ::
  Members '[Error ScopeError, Reader ScopeParameters, Files, Parser.InfoTableBuilder] r =>
  TopModulePath ->
  Sem r (Module 'Parsed 'ModuleTop)
readParseModule mp = do
  path <- modulePathToFilePath mp
  txt <- readFile' path
  root <- asks (^. scopeRootPath)
  case runModuleParser root path txt of
    Left err -> throw (ErrParser (MegaParsecError err))
    Right (tbl, m) -> Parser.mergeTable tbl $> m

modulePathToFilePath ::
  Members '[Reader ScopeParameters] r =>
  TopModulePath ->
  Sem r FilePath
modulePathToFilePath mp = do
  root <- asks (^. scopeRootPath)
  ext <- asks (^. scopeFileExtension)
  return $ topModulePathToFilePath' (Just ext) root mp

checkOperatorSyntaxDef ::
  forall r.
  Members '[Error ScopeError, State Scope] r =>
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
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature TypeSignature {..} = do
  sigType' <- checkParseExpressionAtoms _sigType
  sigName' <- bindFunctionSymbol _sigName
  registerFunction' TypeSignature {_sigName = sigName', _sigType = sigType', ..}

checkConstructorDef ::
  Members '[Error ScopeError, Reader LocalVars, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  InductiveConstructorDef 'Parsed ->
  Sem r (InductiveConstructorDef 'Scoped)
checkConstructorDef InductiveConstructorDef {..} = do
  constructorType' <- checkParseExpressionAtoms _constructorType
  constructorName' <- bindConstructorSymbol _constructorName
  registerConstructor'
    InductiveConstructorDef
      { _constructorName = constructorName',
        _constructorType = constructorType'
      }

withParams ::
  forall r a.
  Members '[Reader LocalVars, Error ScopeError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  [InductiveParameter 'Parsed] ->
  ([InductiveParameter 'Scoped] -> Sem r a) ->
  Sem r a
withParams xs a = go [] xs
  where
    go :: [InductiveParameter 'Scoped] -> [InductiveParameter 'Parsed] -> Sem r a
    go inductiveParameters' params =
      case params of
        -- More params to check
        (InductiveParameter {..} : ps) -> do
          inductiveParameterType' <- checkParseExpressionAtoms _inductiveParameterType
          inductiveParameterName' <- freshVariable _inductiveParameterName
          let param' =
                InductiveParameter
                  { _inductiveParameterType = inductiveParameterType',
                    _inductiveParameterName = inductiveParameterName'
                  }
          withBindLocalVariable (LocalVariable inductiveParameterName') $
            go (inductiveParameters' ++ [param']) ps
        -- All params have been checked
        [] -> a inductiveParameters'

checkInductiveDef ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  InductiveDef 'Parsed ->
  Sem r (InductiveDef 'Scoped)
checkInductiveDef InductiveDef {..} = do
  withParams _inductiveParameters $ \inductiveParameters' -> do
    inductiveType' <- mapM checkParseExpressionAtoms _inductiveType
    inductiveName' <- bindInductiveSymbol _inductiveName
    inductiveConstructors' <- mapM checkConstructorDef _inductiveConstructors
    registerInductive'
      InductiveDef
        { _inductiveName = inductiveName',
          _inductiveParameters = inductiveParameters',
          _inductiveType = inductiveType',
          _inductiveConstructors = inductiveConstructors'
        }

checkTopModule_ ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Files, State ScoperState, InfoTableBuilder, Parser.InfoTableBuilder, NameIdGen] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule_ = fmap (^. moduleRefModule) . checkTopModule

checkTopModule ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Files, State ScoperState, InfoTableBuilder, Parser.InfoTableBuilder, NameIdGen] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (ModuleRef'' 'S.NotConcrete 'ModuleTop)
checkTopModule m@(Module path params body) = do
  checkPath
  r <- checkedModule
  modify (over (scoperModulesCache . cachedModules) (HashMap.insert path r))
  return r
  where
    checkPath :: Members '[Files, Reader ScopeParameters, Error ScopeError] s => Sem s ()
    checkPath = do
      expectedPath <- modulePathToFilePath path
      let actualPath = getLoc path ^. intFile
      unlessM (fromMaybe True <$> equalPaths' expectedPath actualPath) $
        throw
          ( ErrWrongTopModuleName
              WrongTopModuleName
                { _wrongTopModuleNameActualName = path,
                  _wrongTopModuleNameExpectedPath = expectedPath,
                  _wrongTopModuleNameActualPath = actualPath
                }
          )
    freshTopModulePath ::
      forall s.
      Members '[State ScoperState, NameIdGen] s =>
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
      -- registerName moduleName
      return moduleName
    iniScope :: Scope
    iniScope = emptyScope (getTopModulePath m)
    checkedModule :: Sem r (ModuleRef'' 'S.NotConcrete 'ModuleTop)
    checkedModule = do
      evalState iniScope $ do
        path' <- freshTopModulePath
        localScope $
          withParams params $ \params' -> do
            (_moduleExportInfo, body') <- checkModuleBody body
            let _moduleRefModule =
                  Module
                    { _modulePath = path',
                      _moduleParameters = params',
                      _moduleBody = body'
                    }
                _moduleRefName = set S.nameConcrete () path'
            return ModuleRef'' {..}

withScope :: Members '[State Scope] r => Sem r a -> Sem r a
withScope ma = do
  before <- get @Scope
  x <- ma
  put before
  return x

checkModuleBody ::
  forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScoperState, Files, Reader LocalVars, InfoTableBuilder, Parser.InfoTableBuilder, NameIdGen] r =>
  [Statement 'Parsed] ->
  Sem r (ExportInfo, [Statement 'Scoped])
checkModuleBody body = do
  body' <- mapM checkStatement body
  checkOrphanFixities
  checkClausesExist body'
  exported <- get >>= exportScope
  return (exported, body')

checkLocalModule ::
  forall r.
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScoperState, Files, Reader LocalVars, InfoTableBuilder, Parser.InfoTableBuilder, NameIdGen] r =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule Module {..} = do
  (_moduleExportInfo, moduleBody', moduleParameters') <-
    withScope $
      withParams _moduleParameters $ \p' -> do
        inheritScope
        (e, b) <- checkModuleBody _moduleBody
        return (e, b, p')
  _modulePath' <- reserveSymbolOf S.KNameLocalModule _modulePath
  let moduleId = _modulePath' ^. S.nameId
      _moduleRefName = set S.nameConcrete () _modulePath'
      _moduleRefModule =
        Module
          { _modulePath = _modulePath',
            _moduleParameters = moduleParameters',
            _moduleBody = moduleBody'
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

checkClausesExist :: forall r. Members '[Error ScopeError, State Scope] r => [Statement 'Scoped] -> Sem r ()
checkClausesExist ss = whenJust msig (throw . ErrLacksFunctionClause . LacksFunctionClause)
  where
    msig =
      listToMaybe
        [ ts | StatementTypeSignature ts <- ss, null
                                                  [c | StatementFunctionClause c <- ss, c ^. clauseOwnerFunction == ts ^. sigName]
        ]

checkOrphanFixities :: forall r. Members '[Error ScopeError, State Scope] r => Sem r ()
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
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
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
      NameQualified (QualifiedName (Path p) s) -> (toList p, s)

getModuleRef :: SymbolEntry -> Maybe (ModuleRef' 'S.NotConcrete)
getModuleRef = \case
  EntryModule m -> Just m
  _ -> Nothing

getExportInfo ::
  forall r.
  Members '[State ScoperState] r =>
  S.ModuleNameId ->
  Sem r ExportInfo
getExportInfo modId = do
  l <-
    HashMap.lookupDefault impossible modId
      <$> gets (^. scoperModules)
  return $ case l ^. unModuleRef' of
    _ :&: ent -> ent ^. moduleExportInfo

checkOpenModule ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
checkOpenModule OpenModule {..} = do
  openModuleName'@(ModuleRef' (_ :&: moduleRef'')) <- lookupModuleSymbol _openModuleName
  openParameters' <- mapM checkParseExpressionAtoms _openParameters
  mergeScope (alterScope (moduleRef'' ^. moduleExportInfo))
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

checkWhereBlock ::
  forall r.
  Members
    '[ Error ScopeError,
       State Scope,
       State ScoperState,
       Reader LocalVars,
       InfoTableBuilder,
       NameIdGen
     ]
    r =>
  WhereBlock 'Parsed ->
  Sem r (WhereBlock 'Scoped)
checkWhereBlock WhereBlock {..} = WhereBlock <$> mapM checkWhereClause whereClauses

checkWhereClause ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  WhereClause 'Parsed ->
  Sem r (WhereClause 'Scoped)
checkWhereClause c = case c of
  WhereOpenModule o -> WhereOpenModule <$> checkOpenModule o
  WhereTypeSig s -> WhereTypeSig <$> checkTypeSignature s
  WhereFunClause f -> WhereFunClause <$> checkFunctionClause f

checkFunctionClause ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  FunctionClause 'Parsed ->
  Sem r (FunctionClause 'Scoped)
checkFunctionClause clause@FunctionClause {..} = do
  clauseOwnerFunction' <- checkTypeSigInScope
  registerName (S.unqualifiedSymbol clauseOwnerFunction')
  (clausePatterns', clauseWhere', clauseBody') <- do
    clp <- mapM checkParsePatternAtom _clausePatterns
    withBindCurrentGroup $ do
      s <- get @Scope
      clw <- mapM checkWhereBlock _clauseWhere
      clb <- checkParseExpressionAtoms _clauseBody
      put s
      return (clp, clw, clb)
  registerFunctionClause'
    FunctionClause
      { _clauseOwnerFunction = clauseOwnerFunction',
        _clausePatterns = clausePatterns',
        _clauseBody = clauseBody',
        _clauseWhere = clauseWhere'
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
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Symbol ->
  Sem r (Maybe SymbolEntry)
lookupLocalEntry sym = do
  ms <- HashMap.lookup sym <$> gets (^. scopeSymbols)
  path <- gets (^. scopePath)
  -- The symbol must be defined in the same path
  return $ do
    SymbolInfo {..} <- ms
    HashMap.lookup path _symbolInfo

localScope :: Sem (Reader LocalVars : r) a -> Sem r a
localScope = runReader (LocalVars mempty)

checkAxiomDef ::
  Members '[InfoTableBuilder, Error ScopeError, State Scope, State ScoperState, NameIdGen] r =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiomDef AxiomDef {..} = do
  axiomType' <- localScope (checkParseExpressionAtoms _axiomType)
  axiomName' <- bindAxiomSymbol _axiomName
  registerAxiom' AxiomDef {_axiomName = axiomName', _axiomType = axiomType', ..}

checkCompile ::
  Members '[InfoTableBuilder, Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
  Compile 'Parsed ->
  Sem r (Compile 'Scoped)
checkCompile c@Compile {..} = do
  scopedSym :: S.Symbol <- checkCompileName c
  let sym :: Symbol = c ^. compileName
  rules <- gets (^. scopeCompilationRules)
  if
      | HashMap.member sym rules ->
          throw
            ( ErrMultipleCompileBlockSameName
                (MultipleCompileBlockSameName sym)
            )
      | otherwise -> do
          _ <- checkBackendItems sym _compileBackendItems mempty
          registerName (S.unqualifiedSymbol scopedSym)
          modify
            ( over
                scopeCompilationRules
                (HashMap.insert _compileName (CompileInfo _compileBackendItems))
            )
          registerCompile' $ Compile {_compileName = scopedSym, ..}

checkBackendItems ::
  Members '[Error ScopeError] r =>
  Symbol ->
  [BackendItem] ->
  HashSet Backend ->
  Sem r (HashSet Backend)
checkBackendItems _ [] bset = return bset
checkBackendItems sym (b : bs) bset =
  let cBackend = b ^. backendItemBackend
   in if
          | HashSet.member cBackend bset ->
              throw
                ( ErrMultipleCompileRuleSameBackend
                    (MultipleCompileRuleSameBackend b sym)
                )
          | otherwise -> checkBackendItems sym bs (HashSet.insert cBackend bset)

checkCompileName ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState, InfoTableBuilder] r =>
  Compile 'Parsed ->
  Sem r S.Symbol
checkCompileName Compile {..} = do
  let sym :: Symbol = _compileName
  let name :: Name = NameUnqualified sym
  scope <- get
  locals <- ask
  entries <- lookupQualifiedSymbol ([], sym)
  case filter S.canBeCompiled entries of
    [] -> case entries of
      [] -> throw (ErrSymNotInScope (NotInScope sym locals scope))
      (e : _) ->
        throw
          ( ErrWrongKindExpressionCompileBlock
              (WrongKindExpressionCompileBlock e)
          )
    [x] -> do
      actualPath <- gets (^. scopePath)
      let scoped = entryToSymbol x sym
      let expectedPath = scoped ^. S.nameDefinedIn
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
  Members '[Error ScopeError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  Eval 'Parsed ->
  Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> localScope (checkParseExpressionAtoms s)

checkPrint ::
  Members '[Error ScopeError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  Print 'Parsed ->
  Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> localScope (checkParseExpressionAtoms s)

checkFunction ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction Function {..} = do
  funParameter' <- checkParam
  let scoped = case funParameter' ^. paramName of
        Nothing -> id
        Just s -> withBindLocalVariable (LocalVariable s)
  funReturn' <- scoped (checkParseExpressionAtoms _funReturn)
  return
    Function
      { _funParameter = funParameter',
        _funReturn = funReturn'
      }
  where
    checkParam :: Sem r (FunctionParameter 'Scoped)
    checkParam = do
      paramType' <- checkParseExpressionAtoms _paramType
      paramName' <- checkParamName
      return
        FunctionParameter
          { _paramName = paramName',
            _paramUsage = _paramUsage,
            _paramType = paramType'
          }
      where
        FunctionParameter {..} = _funParameter
        checkParamName :: Sem r (Maybe S.Symbol)
        checkParamName = case _paramName of
          Nothing -> return Nothing
          Just s -> Just <$> freshVariable s

checkLetClause ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  LetClause 'Parsed ->
  Sem r (LetClause 'Scoped)
checkLetClause lc = case lc of
  LetTypeSig t -> LetTypeSig <$> checkTypeSignature t
  LetFunClause c -> LetFunClause <$> checkFunctionClause c

checkLetBlock ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
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
        _letExpression = letExpression'
      }

checkLambda ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda Lambda {..} = Lambda <$> mapM checkLambdaClause lambdaClauses

checkLambdaClause ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  LambdaClause 'Parsed ->
  Sem r (LambdaClause 'Scoped)
checkLambdaClause LambdaClause {..} = do
  lambdaParameters' <- mapM checkParsePatternAtom lambdaParameters
  lambdaBody' <- withBindCurrentGroup (checkParseExpressionAtoms lambdaBody)
  return
    LambdaClause
      { lambdaParameters = lambdaParameters',
        lambdaBody = lambdaBody'
      }

scopedVar ::
  Members '[InfoTableBuilder] r =>
  LocalVariable ->
  Symbol ->
  Sem r S.Symbol
scopedVar (LocalVariable s) n = do
  let scoped = set S.nameConcrete n s
  registerName (S.unqualifiedSymbol scoped)
  return scoped

checkUnqualified ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState, InfoTableBuilder] r =>
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
  Members '[Error ScopeError, State Scope, NameIdGen, State ScoperState, InfoTableBuilder] r =>
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
      NameQualified (QualifiedName (Path p) s) -> (toList p, s)
      NameUnqualified s -> ([], s)
    -- check whether the symbol is a constructor in scope
    getConstructorRef :: Sem r (Maybe ConstructorRef)
    getConstructorRef = do
      entries <- mapMaybe getConstructor <$> lookupQualifiedSymbol (path, sym)
      case entries of
        [] -> case Path <$> nonEmpty path of
          Nothing -> return Nothing -- There is no constructor with such a name
          Just pth -> throw (ErrQualSymNotInScope (QualifiedName pth sym))
        [e] -> return (Just (set (constructorRefName . S.nameConcrete) n e)) -- There is one constructor with such a name
        es -> throw (ErrAmbiguousSym (AmbiguousSym n (map EntryConstructor es)))
    getConstructor :: SymbolEntry -> Maybe (ConstructorRef' 'S.NotConcrete)
    getConstructor = \case
      EntryConstructor r -> Just r
      _ -> Nothing

withBindCurrentGroup ::
  Members '[State Scope, Reader LocalVars] r =>
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
  Members '[Reader LocalVars] r =>
  LocalVariable ->
  Sem r a ->
  Sem r a
withBindLocalVariable var = local (addLocalVars [var])

-- | Binds a local variable in a bind group, i.e. a group of pattern.
groupBindLocalVariable ::
  forall r.
  Members '[Error ScopeError, State Scope, NameIdGen] r =>
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
  Members '[Error ScopeError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  PatternAtoms 'Parsed ->
  Sem r (PatternAtoms 'Scoped)
checkPatternAtoms (PatternAtoms s) = PatternAtoms <$> mapM checkPatternAtom s

checkPatternAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  PatternAtom 'Parsed ->
  Sem r (PatternAtom 'Scoped)
checkPatternAtom p = case p of
  PatternAtomWildcard -> return PatternAtomWildcard
  PatternAtomEmpty -> return PatternAtomEmpty
  PatternAtomParens e -> PatternAtomParens <$> checkPatternAtoms e
  PatternAtomIden n -> PatternAtomIden <$> checkPatternName n

checkName ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState, InfoTableBuilder] r =>
  Name ->
  Sem r ScopedIden
checkName n = case n of
  NameQualified q -> checkQualifiedExpr q
  NameUnqualified s -> checkUnqualified s

checkExpressionAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtom 'Parsed ->
  Sem r (ExpressionAtom 'Scoped)
checkExpressionAtom e = case e of
  AtomIdentifier n -> AtomIdentifier <$> checkName n
  AtomLambda lam -> AtomLambda <$> checkLambda lam
  AtomLetBlock letBlock -> AtomLetBlock <$> checkLetBlock letBlock
  AtomUniverse uni -> return (AtomUniverse uni)
  AtomFunction fun -> AtomFunction <$> checkFunction fun
  AtomParens par -> AtomParens <$> checkParens par
  AtomFunArrow -> return AtomFunArrow
  AtomLiteral l -> return (AtomLiteral l)
  AtomMatch match -> AtomMatch <$> checkMatch match

checkParens ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParens e@(ExpressionAtoms as) = case as of
  AtomIdentifier s :| [] -> do
    scopedId <- checkName s
    let scopedIdenNoFix = idenOverName (set S.nameFixity Nothing) scopedId
    return (ExpressionParensIdentifier scopedIdenNoFix)
  _ -> checkParseExpressionAtoms e

checkMatchAlt ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  MatchAlt 'Parsed ->
  Sem r (MatchAlt 'Scoped)
checkMatchAlt MatchAlt {..} = do
  matchAltPattern' <- checkParsePatternAtom matchAltPattern
  matchAltBody' <- withBindCurrentGroup (checkParseExpressionAtoms matchAltBody)
  return
    MatchAlt
      { matchAltPattern = matchAltPattern',
        matchAltBody = matchAltBody'
      }

checkMatch ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  Match 'Parsed ->
  Sem r (Match 'Scoped)
checkMatch Match {..} = do
  matchExpression' <- checkParseExpressionAtoms matchExpression
  matchAlts' <- mapM checkMatchAlt matchAlts
  return
    Match
      { matchExpression = matchExpression',
        matchAlts = matchAlts'
      }

checkExpressionAtoms ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l) = ExpressionAtoms <$> mapM checkExpressionAtom l

checkParseExpressionAtoms ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  PatternAtom 'Parsed ->
  Sem r Pattern
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkStatement ::
  Members '[Error ScopeError, Reader ScopeParameters, Files, State Scope, State ScoperState, Reader LocalVars, InfoTableBuilder, Parser.InfoTableBuilder, NameIdGen] r =>
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
  StatementEval e -> StatementEval <$> checkEval e
  StatementPrint e -> StatementPrint <$> checkPrint e
  StatementForeign d -> return (StatementForeign d)
  StatementCompile c -> StatementCompile <$> checkCompile c

-------------------------------------------------------------------------------
-- Infix Expression
-------------------------------------------------------------------------------
makeExpressionTable2 ::
  ExpressionAtoms 'Scoped -> [[P.Operator Parse Expression]]
makeExpressionTable2 (ExpressionAtoms atoms) = [appOp] : operators ++ [[functionOp]]
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
    appOp :: P.Operator Parse Expression
    appOp = P.InfixL (return app)
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
              { _funParameter = param,
                _funReturn = b
              }
          where
            param =
              FunctionParameter
                { _paramName = Nothing,
                  _paramUsage = Nothing,
                  _paramType = a
                }

parseExpressionAtoms ::
  Members '[Error ScopeError, State Scope] r =>
  ExpressionAtoms 'Scoped ->
  Sem r Expression
parseExpressionAtoms a@(ExpressionAtoms sections) = do
  case res of
    Left {} ->
      throw
        ( ErrInfixParser
            InfixError {_infixErrAtoms = a}
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

parseTerm :: Members '[Embed Parse] r => Sem r Expression
parseTerm =
  embed @Parse $
    parseUniverse
      <|> parseNoInfixIdentifier
      <|> parseParens
      <|> parseFunction
      <|> parseLambda
      <|> parseLiteral
      <|> parseMatch
      <|> parseLetBlock
  where
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

    parseMatch :: Parse Expression
    parseMatch = ExpressionMatch <$> P.token match mempty
      where
        match :: ExpressionAtom 'Scoped -> Maybe (Match 'Scoped)
        match s = case s of
          AtomMatch l -> Just l
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

type ParsePat = P.Parsec () [PatternAtom 'Scoped]

makePatternTable ::
  PatternAtom 'Scoped -> [[P.Operator ParsePat Pattern]]
makePatternTable atom = [appOp] : operators
  where
    getConstructorRef :: PatternAtom 'Scoped -> Maybe ConstructorRef
    getConstructorRef s = case s of
      PatternAtomIden iden -> case iden of
        PatternScopedConstructor r -> Just r
        PatternScopedVar {} -> Nothing
      _ -> Nothing
    operators = mkSymbolTable constructorRefs
    constructorRefs :: [ConstructorRef]
    constructorRefs = case atom of
      PatternAtomParens (PatternAtoms atoms) -> mapMaybe getConstructorRef (toList atoms)
      _ -> []
    mkSymbolTable :: [ConstructorRef] -> [[P.Operator ParsePat Pattern]]
    mkSymbolTable = reverse . map (map snd) . groupSortOn' fst . mapMaybe unqualifiedSymbolOp
      where
        unqualifiedSymbolOp :: ConstructorRef -> Maybe (Precedence, P.Operator ParsePat Pattern)
        unqualifiedSymbolOp (ConstructorRef' S.Name' {..})
          | Just Fixity {..} <- _nameFixity,
            _nameKind == S.KNameConstructor = Just $
              case _fixityArity of
                Unary u -> (_fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                  where
                    unaryApp :: ConstructorRef -> Pattern -> Pattern
                    unaryApp funName = case u of
                      AssocPostfix -> PatternPostfixApplication . (`PatternPostfixApp` funName)
                Binary b -> (_fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId _nameId))
                  where
                    binaryInfixApp :: ConstructorRef -> Pattern -> Pattern -> Pattern
                    binaryInfixApp name argLeft = PatternInfixApplication . PatternInfixApp argLeft name
                    infixLRN :: ParsePat (Pattern -> Pattern -> Pattern) -> P.Operator ParsePat Pattern
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
    appOp :: P.Operator ParsePat Pattern
    appOp = P.InfixL (return app)
      where
        app :: Pattern -> Pattern -> Pattern
        app l = PatternApplication . PatternApp l

parsePatternTerm ::
  forall r.
  Members '[Reader (ParsePat Pattern), Embed ParsePat] r =>
  Sem r Pattern
parsePatternTerm = do
  pPat <- ask
  embed @ParsePat $
    parseNoInfixConstructor
      <|> parseVariable
      <|> parseParens pPat
      <|> parseWildcard
      <|> parseEmpty
  where
    parseNoInfixConstructor :: ParsePat Pattern
    parseNoInfixConstructor =
      PatternConstructor
        <$> P.token constructorNoFixity mempty
      where
        constructorNoFixity :: PatternAtom 'Scoped -> Maybe ConstructorRef
        constructorNoFixity s = case s of
          PatternAtomIden (PatternScopedConstructor ref)
            | not (S.hasFixity n) -> Just ref
            where
              n = ref ^. constructorRefName
          _ -> Nothing

    parseWildcard :: ParsePat Pattern
    parseWildcard = PatternWildcard <$ P.satisfy isWildcard
      where
        isWildcard :: PatternAtom 'Scoped -> Bool
        isWildcard s = case s of
          PatternAtomWildcard -> True
          _ -> False

    parseEmpty :: ParsePat Pattern
    parseEmpty = PatternEmpty <$ P.satisfy isEmpty
      where
        isEmpty :: PatternAtom 'Scoped -> Bool
        isEmpty s = case s of
          PatternAtomEmpty -> True
          _ -> False

    parseVariable :: ParsePat Pattern
    parseVariable = PatternVariable <$> P.token var mempty
      where
        var :: PatternAtom 'Scoped -> Maybe S.Symbol
        var s = case s of
          PatternAtomIden (PatternScopedVar sym) -> Just sym
          _ -> Nothing

    parseParens :: ParsePat Pattern -> ParsePat Pattern
    parseParens patternParser = do
      exprs <- P.token parenPat mempty
      case P.parse patternParser strPath exprs of
        Right r -> return r
        Left {} -> mzero
      where
        strPath :: FilePath
        strPath = "inner parens"
        parenPat :: PatternAtom 'Scoped -> Maybe [PatternAtom 'Scoped]
        parenPat s = case s of
          PatternAtomParens (PatternAtoms ss) -> Just (toList ss)
          _ -> Nothing

mkPatternParser ::
  forall r.
  Members '[Embed ParsePat] r =>
  [[P.Operator ParsePat Pattern]] ->
  Sem r Pattern
mkPatternParser table = embed @ParsePat pPattern
  where
    pPattern :: ParsePat Pattern
    pPattern = P.makeExprParser pTerm table
    pTerm :: ParsePat Pattern
    pTerm = runM parseTermRec
      where
        parseTermRec :: Sem '[Embed ParsePat] Pattern
        parseTermRec = runReader pPattern parsePatternTerm

parsePatternAtom ::
  Members '[Error ScopeError, State Scope] r => PatternAtom 'Scoped -> Sem r Pattern
parsePatternAtom sec = do
  case res of
    Left {} -> throw (ErrInfixPattern (InfixErrorP sec))
    Right r -> return r
  where
    tbl = makePatternTable sec
    parser :: ParsePat Pattern
    parser = runM (mkPatternParser tbl) <* P.eof
    res = P.parse parser filePath [sec]
    filePath :: FilePath
    filePath = "tmp"
