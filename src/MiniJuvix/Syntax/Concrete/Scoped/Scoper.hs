-- | Limitations:
-- 1. A symbol introduced by a type signature can only be used once per Module.
--
module MiniJuvix.Syntax.Concrete.Scoped.Scoper where

import qualified Control.Monad.Combinators.Expr as P
import Data.Functor.Identity
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Stream as Stream
import Lens.Micro.Platform
import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Concrete.Base as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Parser (runModuleParser)
import MiniJuvix.Syntax.Concrete.Scoped.Error
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import qualified MiniJuvix.Syntax.Concrete.Name as N
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import MiniJuvix.Syntax.Concrete.Scoped.Scoper.Files

--------------------------------------------------------------------------------

scopeCheck1IO :: FilePath -> Module 'Parsed 'ModuleTop -> IO (Either ScopeError (Module 'Scoped 'ModuleTop))
scopeCheck1IO root = runFinal . embedToFinal @IO . runFilesIO . fixpointToFinal @IO . scopeCheck1 root

scopeCheck1Pure :: HashMap FilePath Text -> FilePath -> Module 'Parsed 'ModuleTop -> Either ScopeError (Module 'Scoped 'ModuleTop)
scopeCheck1Pure fs root = runIdentity . runFinal . runFilesPure fs . fixpointToFinal @Identity . scopeCheck1 root

scopeCheck1 ::
  Members [Files, Fixpoint] r =>
  FilePath ->
  Module 'Parsed 'ModuleTop ->
  Sem r (Either ScopeError (Module 'Scoped 'ModuleTop))
scopeCheck1 root m = fmap head <$> scopeCheck root (pure m)

scopeCheck ::
  Members [Files, Fixpoint] r =>
  FilePath ->
  NonEmpty (Module 'Parsed 'ModuleTop) ->
  Sem r (Either ScopeError (NonEmpty (Module 'Scoped 'ModuleTop)))
scopeCheck root modules =
  runError $
    runReader scopeParameters $
      evalState iniScoperState $
        mapM checkTopModule_ modules
  where
    iniScoperState :: ScoperState
    iniScoperState =
      ScoperState
        { _scoperModulesCache = ModulesCache mempty,
          _scoperFreeNames = S.allNameIds,
          _scoperModules = mempty
        }
    scopeParameters :: ScopeParameters
    scopeParameters =
      ScopeParameters
        { _scopeRootPath = root,
          _scopeFileExtension = ".mjuvix",
          _scopeTopParents = mempty
        }

freshNameId ::
  Members '[State ScoperState] r =>
  Sem r S.NameId
freshNameId = do
  i <- gets (Stream.head . _scoperFreeNames)
  modify (over scoperFreeNames Stream.tail)
  return i

freshVariable :: Members '[State ScoperState, State Scope] r => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol S.KNameLocal

freshSymbol ::
  forall r.
  Members '[State ScoperState, State Scope] r =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
freshSymbol _nameKind _nameConcrete = do
  _nameId <- freshNameId
  _nameDefinedIn <- gets _scopePath
  let _nameDefined = getLoc _nameConcrete
      _nameWhyInScope = S.BecauseDefined
      _namePublicAnn = NoPublic
      _nameVerbatim = _symbolText _nameConcrete
  _nameFixity <- fixity
  return S.Name' {..}
  where
    fixity :: Sem r (Maybe Fixity)
    fixity
      | S.canHaveFixity _nameKind =
        fmap opFixity . HashMap.lookup _nameConcrete <$> gets _scopeFixities
      | otherwise = return Nothing

reserveSymbolOf ::
  forall r.
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  S.NameKind ->
  Symbol ->
  Sem r S.Symbol
reserveSymbolOf k s = do
  checkNotBound
  freshSymbol k s
  where
    checkNotBound :: Sem r ()
    checkNotBound = do
      path <- gets _scopePath
      syms <- gets _scopeSymbols
      let exists = HashMap.lookup s syms >>= HashMap.lookup path . _symbolInfo
      whenJust exists $
        \e ->
          throw
            ( ErrMultipleDeclarations
                MultipleDeclarations
                  { _multipleDeclEntry = e,
                    _multipleDeclSymbol = _symbolText s,
                    _multipleDeclSecond = getLoc s
                  }
            )

bindReservedSymbol ::
  Members '[State Scope] r =>
  S.Symbol ->
  SymbolEntry ->
  Sem r ()
bindReservedSymbol s' entry = do
  path <- gets _scopePath
  modify (over scopeSymbols (HashMap.alter (Just . addS path) s))
  where
    s = S._nameConcrete s'
    addS :: S.AbsModulePath -> Maybe SymbolInfo -> SymbolInfo
    addS path m = case m of
      Nothing -> symbolInfoSingle entry
      Just SymbolInfo {..} -> SymbolInfo (HashMap.insert path entry _symbolInfo)

bindSymbolOf ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  S.NameKind ->
  (S.Name' () -> SymbolEntry) ->
  Symbol ->
  Sem r S.Symbol
bindSymbolOf k e s = do
  s' <- reserveSymbolOf k s
  bindReservedSymbol s' (e (set S.nameConcrete () s'))
  return s'

bindFunctionSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  Expression ->
  Symbol ->
  Sem r S.Symbol
bindFunctionSymbol sig =
  bindSymbolOf
    S.KNameFunction
    (\s' -> EntryFunction (FunctionRef' s' sig))

bindInductiveSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  InductiveDef 'Scoped ->
  Symbol ->
  Sem r S.Symbol
bindInductiveSymbol def =
  bindSymbolOf
    S.KNameInductive
    (\s' -> EntryInductive (InductiveRef' s' def))

bindAxiomSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  [BackendItem] ->
  Expression ->
  Symbol ->
  Sem r S.Symbol
bindAxiomSymbol backends ty =
  bindSymbolOf
    S.KNameAxiom
    (\s' -> EntryAxiom (AxiomRef' s' ty backends))

bindConstructorSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
  Expression ->
  Symbol ->
  Sem r S.Symbol
bindConstructorSymbol sig =
  bindSymbolOf
    S.KNameConstructor
    (\s' -> EntryConstructor (ConstructorRef' s' sig))

bindLocalModuleSymbol ::
  Members '[Error ScopeError, State ScoperState, State Scope] r =>
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
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, Files, State ScoperState, Fixpoint] r =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
checkImport import_@(Import path) = do
  checkCycle
  cache <- gets (_cachedModules . _scoperModulesCache)
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
      topp <- asks _scopeTopParents
      case span (/= import_) topp of
        (_, []) -> return ()
        (c, _) ->
          let cyc = NonEmpty.reverse (import_ :| c)
           in throw (ErrImportCycle (ImportCycle cyc))

getTopModulePath :: Module 'Parsed 'ModuleTop -> S.AbsModulePath
getTopModulePath Module {..} =
  S.AbsModulePath
    { S.absTopModulePath = _modulePath,
      S.absLocalPath = mempty
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
          r <- HashMap.lookup final <$> gets _scopeSymbols
          return $ case r of
            Nothing -> []
            Just SymbolInfo {..} -> toList _symbolInfo
        (p : ps) ->
          mapMaybe (lookInExport final ps . getModuleExportInfo) . concat . maybeToList . fmap (mapMaybe getModuleRef . toList . _symbolInfo)
            . HashMap.lookup p
            <$> gets _scopeSymbols
    importedTopModule :: Sem r (Maybe SymbolEntry)
    importedTopModule = do
      fmap (EntryModule . mkModuleRef') . HashMap.lookup path <$> gets _scopeTopModules
      where
        path = TopModulePath modules final

lookInExport :: Symbol -> [Symbol] -> ExportInfo -> Maybe SymbolEntry
lookInExport sym remaining e = case remaining of
  [] -> HashMap.lookup sym (_exportSymbols e)
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
          ((fmap (^. moduleExportInfo) . HashMap.lookup topPath) >=> lookInExport sym remaining) <$> gets _scopeTopModules

checkQualifiedExpr ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  QualifiedName ->
  Sem r ScopedIden
checkQualifiedExpr q@(QualifiedName (Path p) sym) = do
  es <- lookupQualifiedSymbol (toList p, sym)
  case es of
    [] -> notInScope
    [e] -> return (entryToScopedIden q' e)
    _ -> throw (ErrAmbiguousSym (AmbiguousSym q' es))
  where
    q' = NameQualified q
    notInScope = throw (ErrQualSymNotInScope q)

unqualifiedSName :: S.Symbol -> S.Name
unqualifiedSName = over S.nameConcrete NameUnqualified

entryToScopedIden :: Name -> SymbolEntry -> ScopedIden
entryToScopedIden name = \case
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
        shouldExport ent =
          _nameDefinedIn == _scopePath
            || _namePublicAnn == Public
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
  Members '[Error ScopeError, Reader ScopeParameters, Files] r =>
  TopModulePath ->
  Sem r (Module 'Parsed 'ModuleTop)
readParseModule mp = do
  path <- modulePathToFilePath mp
  txt <- readFile' path
  case runModuleParser path txt of
    Left err -> throw (ErrParser (MegaParsecError err))
    Right r -> return r

modulePathToFilePath ::
  Members '[Reader ScopeParameters] r =>
  TopModulePath ->
  Sem r FilePath
modulePathToFilePath mp = do
  root <- asks _scopeRootPath
  ext <- asks _scopeFileExtension
  return $ topModulePathToFilePath' (Just ext) root mp

checkOperatorSyntaxDef ::
  forall r.
  Members '[Error ScopeError, State Scope] r =>
  OperatorSyntaxDef ->
  Sem r ()
checkOperatorSyntaxDef s@OperatorSyntaxDef {..} = do
  checkNotDefined
  modify (over scopeFixities (HashMap.insert opSymbol s))
  where
    checkNotDefined :: Sem r ()
    checkNotDefined =
      whenJustM
        (HashMap.lookup opSymbol <$> gets _scopeFixities)
        (\s' -> throw (ErrDuplicateFixity (DuplicateFixity s' s)))

checkTypeSignature ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature TypeSignature {..} = do
  sigType' <- checkParseExpressionAtoms _sigType
  sigName' <- bindFunctionSymbol sigType' _sigName
  return
    TypeSignature
      { _sigName = sigName',
        _sigType = sigType'
      }

checkConstructorDef ::
  Members '[Error ScopeError, Reader LocalVars, State Scope, State ScoperState] r =>
  InductiveConstructorDef 'Parsed ->
  Sem r (InductiveConstructorDef 'Scoped)
checkConstructorDef InductiveConstructorDef {..} = do
  constructorType' <- checkParseExpressionAtoms constructorType
  constructorName' <- bindConstructorSymbol constructorType' constructorName
  return
    InductiveConstructorDef
      { constructorName = constructorName',
        constructorType = constructorType'
      }

withParams ::
  forall r a.
  Members '[Reader LocalVars, Error ScopeError, State Scope, State ScoperState] r =>
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
  Members '[Error ScopeError, State Scope, State ScoperState, Fixpoint, Reader LocalVars] r =>
  InductiveDef 'Parsed ->
  Sem r (InductiveDef 'Scoped)
checkInductiveDef InductiveDef {..} = do
  withParams _inductiveParameters $ \inductiveParameters' -> do
    inductiveType' <- sequence (checkParseExpressionAtoms <$> _inductiveType)
    mfix $ \scopedDef -> do
      inductiveName' <- bindInductiveSymbol scopedDef _inductiveName
      inductiveConstructors' <- mapM checkConstructorDef _inductiveConstructors
      return
        InductiveDef
          { _inductiveName = inductiveName',
            _inductiveParameters = inductiveParameters',
            _inductiveType = inductiveType',
            _inductiveConstructors = inductiveConstructors'
          }

checkTopModule_ ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Files, State ScoperState, Fixpoint] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule_ = fmap (^. moduleRefModule) . checkTopModule

checkTopModule ::
  forall r.
  Members '[Error ScopeError, Reader ScopeParameters, Files, State ScoperState, Fixpoint] r =>
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
      Members '[State ScoperState] s =>
      Sem s S.TopModulePath
    freshTopModulePath = do
      _nameId <- freshNameId
      let _nameDefinedIn = S.topModulePathToAbsPath path
          _nameConcrete = path
          _nameDefined = getLoc (_modulePathName path)
          _nameKind = S.KNameTopModule
          _nameFixity = Nothing
          _namePublicAnn = NoPublic
          _nameWhyInScope = S.BecauseDefined
          _nameVerbatim = N.topModulePathToDottedPath path
      return S.Name' {..}
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
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScoperState, Files, Reader LocalVars, Fixpoint] r =>
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
  Members '[Error ScopeError, State Scope, Reader ScopeParameters, State ScoperState, Files, Reader LocalVars, Fixpoint] r =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule Module {..} = do
  (_moduleExportInfo, moduleBody', moduleParameters') <-
    withScope $
      withParams _moduleParameters $ \p' -> do
        inheritScope
        (e, b) <- checkModuleBody _moduleBody
        return (e, b, p')
  mfix $ \scopedModule -> do
    modulePath' <- bindLocalModuleSymbol _moduleExportInfo scopedModule _modulePath
    let moduleId = S._nameId modulePath'
        _moduleRefName = set S.nameConcrete () modulePath'
        _moduleRefModule =
          Module
            { _modulePath = modulePath',
              _moduleParameters = moduleParameters',
              _moduleBody = moduleBody'
            }
        entry :: ModuleRef' 'S.NotConcrete
        entry = mkModuleRef' @'ModuleLocal ModuleRef'' {..}
    modify (over scoperModules (HashMap.insert moduleId entry))
    return _moduleRefModule
  where
    inheritScope :: Sem r ()
    inheritScope = do
      absPath <- (S.<.> _modulePath) <$> gets _scopePath
      modify (set scopePath absPath)
      modify (over scopeSymbols (fmap inheritSymbol))
      modify (set scopeFixities mempty) -- do not inherit fixity declarations
      where
        inheritSymbol :: SymbolInfo -> SymbolInfo
        inheritSymbol (SymbolInfo s) = SymbolInfo (fmap inheritEntry s)
        inheritEntry :: SymbolEntry -> SymbolEntry
        inheritEntry = entryOverName (over S.nameWhyInScope S.BecauseInherited)

checkClausesExist :: forall r. Members '[Error ScopeError, State Scope] r => [Statement 'Scoped] -> Sem r ()
checkClausesExist ss = whenJust msig (throw . ErrLacksFunctionClause . LacksFunctionClause)
  where
    msig =
      listToMaybe
        [ ts | StatementTypeSignature ts <- ss, null
                                                  [ c | StatementFunctionClause c <- ss, c ^. clauseOwnerFunction == ts ^. sigName
                                                  ]
        ]

checkOrphanFixities :: forall r. Members '[Error ScopeError, State Scope] r => Sem r ()
checkOrphanFixities = do
  path <- gets _scopePath
  declared <- gets _scopeFixities
  used <- gets (HashMap.keys . fmap (filter (== path) . HashMap.keys . _symbolInfo) . _scopeSymbols)
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
      <$> gets _scoperModules
  return $ case l ^. unModuleRef' of
    _ :&: ent -> ent ^. moduleExportInfo

checkOpenModule ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
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
        alterEntry = entryOverName (set S.nameWhyInScope S.BecauseImportedOpened . set S.namePublicAnn _openPublic)
        alterEntries :: ExportInfo -> ExportInfo
        alterEntries = over exportSymbols (fmap alterEntry)
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
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  WhereBlock 'Parsed ->
  Sem r (WhereBlock 'Scoped)
checkWhereBlock WhereBlock {..} = WhereBlock <$> mapM checkWhereClause whereClauses

checkWhereClause ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  WhereClause 'Parsed ->
  Sem r (WhereClause 'Scoped)
checkWhereClause c = case c of
  WhereOpenModule o -> WhereOpenModule <$> checkOpenModule o
  WhereTypeSig s -> WhereTypeSig <$> checkTypeSignature s
  WhereFunClause f -> WhereFunClause <$> checkFunctionClause f

checkFunctionClause ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  FunctionClause 'Parsed ->
  Sem r (FunctionClause 'Scoped)
checkFunctionClause clause@FunctionClause {..} = do
  clauseOwnerFunction' <- checkTypeSigInScope
  (clausePatterns', clauseWhere', clauseBody') <- do
    clp <- mapM checkParsePatternAtom _clausePatterns
    withBindCurrentGroup $ do
      s <- get @Scope
      clw <- sequence (checkWhereBlock <$> _clauseWhere)
      clb <- checkParseExpressionAtoms _clauseBody
      put s
      return (clp, clw, clb)
  return
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
  ms <- HashMap.lookup sym <$> gets _scopeSymbols
  path <- gets _scopePath
  -- The symbol must be defined in the same path
  return $ do
    SymbolInfo {..} <- ms
    HashMap.lookup path _symbolInfo

checkAxiomDef ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiomDef AxiomDef {..} = do
  axiomType' <- localScope $ checkParseExpressionAtoms _axiomType
  axiomName' <- bindAxiomSymbol _axiomBackendItems axiomType' _axiomName
  return
    AxiomDef
      { _axiomName = axiomName',
        _axiomType = axiomType',
        ..
      }

localScope :: Sem (Reader LocalVars : r) a -> Sem r a
localScope = runReader (LocalVars mempty)

checkEval ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Eval 'Parsed ->
  Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> localScope (checkParseExpressionAtoms s)

checkPrint ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Print 'Parsed ->
  Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> localScope (checkParseExpressionAtoms s)

checkFunction ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction Function {..} = do
  funParameter' <- checkParam
  let scoped = case paramName funParameter' of
        Nothing -> id
        Just s -> withBindLocalVariable (LocalVariable s)
  funReturn' <- scoped (checkParseExpressionAtoms funReturn)
  return
    Function
      { funParameter = funParameter',
        funReturn = funReturn'
      }
  where
    checkParam :: Sem r (FunctionParameter 'Scoped)
    checkParam = do
      paramType' <- checkParseExpressionAtoms paramType
      paramName' <- checkParamName
      return
        FunctionParameter
          { paramName = paramName',
            paramUsage = paramUsage,
            paramType = paramType'
          }
      where
        FunctionParameter {..} = funParameter
        checkParamName :: Sem r (Maybe S.Symbol)
        checkParamName = case paramName of
          Nothing -> return Nothing
          Just s -> Just <$> freshVariable s

-- | Like a regular type signature?
checkLocalTypeSig ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkLocalTypeSig = checkTypeSignature

checkLetClause ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  LetClause 'Parsed ->
  Sem r (LetClause 'Scoped)
checkLetClause lc = case lc of
  LetTypeSig t -> LetTypeSig <$> checkLocalTypeSig t
  LetFunClause c -> LetFunClause <$> checkFunctionClause c

checkLetBlock ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  LetBlock 'Parsed ->
  Sem r (LetBlock 'Scoped)
checkLetBlock LetBlock {..} = do
  s <- get @Scope -- backup scope: we do not want local definitions to stay in scope
  letClauses' <- mapM checkLetClause letClauses
  letExpression' <- checkParseExpressionAtoms letExpression
  put s -- restore scope
  return
    LetBlock
      { letClauses = letClauses',
        letExpression = letExpression'
      }

checkLambda ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda Lambda {..} = Lambda <$> mapM checkLambdaClause lambdaClauses

checkLambdaClause ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
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

checkUnqualified ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
  Symbol ->
  Sem r ScopedIden
checkUnqualified s = do
  -- Local vars have scope priority
  l <- HashMap.lookup s <$> asks _localVars
  case l of
    Just LocalVariable {..} -> return (ScopedVar variableName)
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
        [x] -> return (entryToScopedIden n x)
        es -> throw (ErrAmbiguousSym (AmbiguousSym n es))
      where
        n = NameUnqualified s

checkPatternName ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Name ->
  Sem r PatternScopedIden
checkPatternName n = do
  c <- getConstructorRef
  case c of
    Just constr -> return (PatternScopedConstructor constr) -- the symbol is a constructor
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
  grp <- gets _scopeBindGroup
  modify (over scopeBindGroup (const mempty)) -- empties the group
  local (over localVars (HashMap.union grp)) ma

addLocalVars :: [LocalVariable] -> LocalVars -> LocalVars
addLocalVars lv = over localVars (flip (foldr insertVar) lv)
  where
    insertVar v = HashMap.insert (S._nameConcrete (variableName v)) v

withBindLocalVariable ::
  Members '[Reader LocalVars] r =>
  LocalVariable ->
  Sem r a ->
  Sem r a
withBindLocalVariable var = local (addLocalVars [var])

-- | Binds a local variable in a bind group, i.e. a group of pattern.
groupBindLocalVariable ::
  forall r.
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  Symbol ->
  Sem r S.Symbol
groupBindLocalVariable s = do
  checkNotInGroup
  addToGroup
  where
    checkNotInGroup :: Sem r ()
    checkNotInGroup =
      whenJustM
        (HashMap.lookup s <$> gets _scopeBindGroup)
        ( \x ->
            throw
              ( ErrBindGroup
                  BindGroupConflict
                    { _bindGroupFirst = S._nameConcrete (variableName x),
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
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  PatternAtoms 'Parsed ->
  Sem r (PatternAtoms 'Scoped)
checkPatternAtoms (PatternAtoms s) = PatternAtoms <$> mapM checkPatternAtom s

checkPatternAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  PatternAtom 'Parsed ->
  Sem r (PatternAtom 'Scoped)
checkPatternAtom p = case p of
  PatternAtomWildcard -> return PatternAtomWildcard
  PatternAtomEmpty -> return PatternAtomEmpty
  PatternAtomParens e -> PatternAtomParens <$> checkPatternAtoms e
  PatternAtomIden n -> PatternAtomIden <$> checkPatternName n

checkName ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
  Name ->
  Sem r ScopedIden
checkName n = case n of
  NameQualified q -> checkQualifiedExpr q
  NameUnqualified s -> checkUnqualified s

checkExpressionAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
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
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParens e@(ExpressionAtoms as) = case as of
  AtomIdentifier s :| [] -> do
    scopedId <- checkName s
    let scopedIdenNoFix = idenOverName (set S.nameFixity Nothing) scopedId
    return (ExpressionParensIdentifier scopedIdenNoFix)
  _ -> checkParseExpressionAtoms e

checkMatchAlt ::
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
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
  Members '[Error ScopeError, State Scope, Reader LocalVars, State ScoperState] r =>
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
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l) = ExpressionAtoms <$> mapM checkExpressionAtom l

checkParseExpressionAtoms ::
  Members '[Error ScopeError, State Scope, State ScoperState, Reader LocalVars] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  Members '[Error ScopeError, State Scope, State ScoperState] r =>
  PatternAtom 'Parsed ->
  Sem r Pattern
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkStatement ::
  Members '[Error ScopeError, Reader ScopeParameters, Files, State Scope, State ScoperState, Reader LocalVars, Fixpoint] r =>
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
  StatementForeign d -> return $ StatementForeign d

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
    mkSymbolTable = reverse . map (map snd) . groupSortOn fst . mapMaybe mkOperator
      where
        mkOperator :: ScopedIden -> Maybe (Precedence, P.Operator Parse Expression)
        mkOperator iden
          | Just Fixity {..} <- _nameFixity = Just $
            case fixityArity of
              Unary u -> (fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                where
                  unaryApp :: ScopedIden -> Expression -> Expression
                  unaryApp funName arg = case u of
                    AssocPostfix -> ExpressionPostfixApplication (PostfixApplication arg funName)
              Binary b -> (fixityPrecedence, infixLRN (binaryApp <$> parseSymbolId _nameId))
                where
                  binaryApp :: ScopedIden -> Expression -> Expression -> Expression
                  binaryApp infixAppOperator infixAppLeft infixAppRight =
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
              { applicationFunction = f,
                applicationParameter = x
              }
    -- Non-dependent function type: A → B
    functionOp :: P.Operator Parse Expression
    functionOp = P.InfixR (nonDepFun <$ P.single AtomFunArrow)
      where
        nonDepFun :: Expression -> Expression -> Expression
        nonDepFun a b =
          ExpressionFunction
            Function
              { funParameter = param,
                funReturn = b
              }
          where
            param =
              FunctionParameter
                { paramName = Nothing,
                  paramUsage = Nothing,
                  paramType = a
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
        lit :: ExpressionAtom 'Scoped -> Maybe Literal
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
    mkSymbolTable = reverse . map (map snd) . groupSortOn fst . mapMaybe unqualifiedSymbolOp
      where
        unqualifiedSymbolOp :: ConstructorRef -> Maybe (Precedence, P.Operator ParsePat Pattern)
        unqualifiedSymbolOp (ConstructorRef' (S.Name' {..}) _)
          | Just Fixity {..} <- _nameFixity,
            _nameKind == S.KNameConstructor = Just $
            case fixityArity of
              Unary u -> (fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                where
                  unaryApp :: ConstructorRef -> Pattern -> Pattern
                  unaryApp funName = case u of
                    AssocPostfix -> PatternPostfixApplication . (`PatternPostfixApp` funName)
              Binary b -> (fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId _nameId))
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

    filePath = "tmp"
