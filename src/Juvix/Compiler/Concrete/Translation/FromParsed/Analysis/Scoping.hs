module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error,
  )
where

import Control.Monad.Combinators.Expr qualified as P
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Gen qualified as G
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.InfoTableBuilder
import Juvix.Compiler.Concrete.Data.Name qualified as N
import Juvix.Compiler.Concrete.Data.NameSignature
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra (fromAmbiguousIterator)
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty (ppTrace)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Data.Context
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context (ParserResult)
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context qualified as Parsed
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.IteratorAttribs
import Juvix.Data.NameKind
import Juvix.Prelude

iniScoperState :: ScoperState
iniScoperState =
  ScoperState
    { _scoperModulesCache = ModulesCache mempty,
      _scoperModules = mempty,
      _scoperScope = mempty,
      _scoperSignatures = mempty
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
  (Members '[Error JuvixError, NameIdGen, State Scope] r) =>
  InfoTable ->
  ExpressionAtoms 'Parsed ->
  Sem r Expression
scopeCheckExpression tab as = mapError (JuvixError @ScoperError) $ do
  fmap snd
    . ignoreHighlightBuilder
    . runInfoTableBuilder tab
    . runReader iniScopeParameters
    . evalState iniScoperState
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
  Members '[Error JuvixError, InfoTableBuilder, NameIdGen, State Scope, Reader ScopeParameters, State ScoperState] r =>
  Import 'Parsed ->
  Sem r (Import 'Scoped)
scopeCheckImport = mapError (JuvixError @ScoperError) . checkImport

scopeCheckOpenModule ::
  forall r.
  Members '[Error JuvixError, InfoTableBuilder, NameIdGen, State Scope, Reader ScopeParameters, State ScoperState] r =>
  OpenModule 'Parsed ->
  Sem r (OpenModule 'Scoped)
scopeCheckOpenModule = mapError (JuvixError @ScoperError) . checkOpenModule

freshVariable :: Members '[NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, State ScoperState] r => Symbol -> Sem r S.Symbol
freshVariable = freshSymbol KNameLocal

checkProjectionDef ::
  forall r.
  Members '[State Scope, State ScoperState, NameIdGen, State ScoperFixities, State ScoperIterators] r =>
  ProjectionDef 'Parsed ->
  Sem r (ProjectionDef 'Scoped)
checkProjectionDef p = do
  _projectionField <- freshSymbol KNameFunction (p ^. projectionField)
  return ProjectionDef {
    _projectionFieldIx = p ^. projectionFieldIx,
    _projectionConstructor = p ^. projectionConstructor,
    _projectionField
                       }

freshSymbol ::
  forall r.
  Members '[State Scope, State ScoperState, NameIdGen, State ScoperFixities, State ScoperIterators] r =>
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
    fixity :: Sem r (Maybe Fixity)
    fixity
      | S.canHaveFixity _nameKind = do
          mf <- gets (^? scoperFixities . at _nameConcrete . _Just . symbolFixityDef . opFixity)
          when (isJust mf) (modify (set (scoperFixities . at _nameConcrete . _Just . symbolFixityUsed) True))
          return mf
      | otherwise = return Nothing

    iter :: Sem r (Maybe IteratorAttribs)
    iter = runFail $ do
      failUnless (S.canBeIterator _nameKind)
      ma <- gets (^? scoperIterators . at _nameConcrete . _Just . symbolIteratorDef . iterAttribs) >>= failMaybe
      let attrs = maybe emptyIteratorAttribs (^. withLocParam . withSourceValue) ma
      modify (set (scoperIterators . at _nameConcrete . _Just . symbolIteratorUsed) True)
      return attrs

reserveSymbolSignatureOf ::
  forall (k :: NameKind) r d.
  ( Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State ScoperIterators, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r,
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
  ( Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State ScoperIterators, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r,
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
  registerName (S.unqualifiedSymbol s')
  let entry :: NameSpaceEntryType (NameKindNameSpace nameKind)
      entry =
        let symE = SymbolEntry (S.unConcrete s')
            modE = ModuleSymbolEntry (S.unConcrete s')
         in case k of
              SKNameConstructor -> symE
              SKNameInductive -> symE
              SKNameFunction -> symE
              SKNameAxiom -> symE
              SKNameLocal -> symE
              SKNameLocalModule -> modE
              SKNameTopModule -> modE
      addS :: NameSpaceEntryType ns -> Maybe (SymbolInfo ns) -> SymbolInfo ns
      addS mentry m = case m of
        Nothing -> symbolInfoSingle mentry
        Just SymbolInfo {..} -> case strat of
          BindingLocal -> symbolInfoSingle mentry
          BindingTop -> SymbolInfo (HashMap.insert path mentry _symbolInfo)
  modify (over scopeNameSpace (HashMap.alter (Just . addS entry) s))
  return s'
  where
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
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State ScoperIterators, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
getReservedDefinitionSymbol s = do
  m <- gets (^. scopeLocalSymbols)
  let s' = fromMaybe err (m ^. at s)
      err = error ("impossible. Contents of scope:\n" <> ppTrace (toList m))
  return s'

ignoreFixities :: Sem (State ScoperFixities ': r) a -> Sem r a
ignoreFixities = evalState mempty

ignoreIterators :: Sem (State ScoperIterators ': r) a -> Sem r a
ignoreIterators = evalState mempty

-- | Variables are assumed to never be infix operators
bindVariableSymbol ::
  Members '[Error ScoperError, NameIdGen, State Scope, InfoTableBuilder, State ScoperState] r =>
  Symbol ->
  Sem r S.Symbol
bindVariableSymbol = localBindings . ignoreFixities . ignoreIterators . reserveSymbolOf SKNameLocal Nothing

reserveInductiveSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r =>
  InductiveDef 'Parsed ->
  Sem r S.Symbol
reserveInductiveSymbol d = reserveSymbolSignatureOf SKNameInductive d (d ^. inductiveName)

reserveProjectionSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, Reader BindingStrategy, InfoTableBuilder, State ScoperState] r =>
   ProjectionDef 'Parsed ->
  Sem r S.Symbol
reserveProjectionSymbol d = reserveSymbolOf SKNameFunction Nothing (d ^. projectionField)

reserveConstructorSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r =>
  InductiveDef 'Parsed ->
  ConstructorDef 'Parsed ->
  Sem r S.Symbol
reserveConstructorSymbol d c = reserveSymbolSignatureOf SKNameConstructor (d, c) (c ^. constructorName)

reserveFunctionSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r =>
  FunctionDef 'Parsed ->
  Sem r S.Symbol
reserveFunctionSymbol f =
  reserveSymbolSignatureOf SKNameFunction f (f ^. signName)

reserveAxiomSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, State ScoperState, Reader BindingStrategy, InfoTableBuilder] r =>
  AxiomDef 'Parsed ->
  Sem r S.Symbol
reserveAxiomSymbol a = reserveSymbolSignatureOf SKNameAxiom a (a ^. axiomName)

bindFunctionSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
bindFunctionSymbol = getReservedDefinitionSymbol

bindInductiveSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
bindInductiveSymbol = getReservedDefinitionSymbol

bindAxiomSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
bindAxiomSymbol = getReservedDefinitionSymbol

bindConstructorSymbol ::
  Members '[Error ScoperError, NameIdGen, State ScoperFixities, State ScoperIterators, State Scope, InfoTableBuilder, State ScoperState, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
bindConstructorSymbol = getReservedDefinitionSymbol

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

-- getModuleRef :: Members '[State ScoperState] r => ModuleSymbolEntry -> Sem r

getModuleExportInfo :: Members '[State ScoperState] r => ModuleSymbolEntry -> Sem r ExportInfo
getModuleExportInfo m = gets (^?! scoperModules . at (m ^. moduleEntry . S.nameId) . _Just . to getModuleRefExportInfo)

-- | Do not call directly. Looks for a symbol in (possibly) nested local modules
lookupSymbolAux ::
  forall r.
  Members '[State ScoperState, State Scope, Output ModuleSymbolEntry, Output SymbolEntry] r =>
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
  Members '[State ScoperState, Output SymbolEntry, Output ModuleSymbolEntry] r =>
  Symbol ->
  [Symbol] ->
  ExportInfo ->
  Sem r ()
lookInExport sym remaining e = case remaining of
  [] -> do
    whenJust (e ^. exportSymbols . at sym) output
    whenJust (e ^. exportModuleSymbols . at sym) output
  s : ss -> whenJustM (mayModule e s) (lookInExport sym ss)
  where
    mayModule :: ExportInfo -> Symbol -> Sem r (Maybe ExportInfo)
    mayModule ExportInfo {..} s =
      mapM getModuleExportInfo (HashMap.lookup s _exportModuleSymbols)

-- | We return a list of entries because qualified names can point to different
-- modules due to nesting.
lookupQualifiedSymbol ::
  forall r.
  Members '[State Scope, State ScoperState] r =>
  ([Symbol], Symbol) ->
  Sem r ([SymbolEntry], [ModuleSymbolEntry])
lookupQualifiedSymbol = runOutputList . execOutputList . go
  where
    go ::
      forall r'.
      Members [State ScoperState, State Scope, Output SymbolEntry, Output ModuleSymbolEntry] r' =>
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

checkQualifiedExpr ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  QualifiedName ->
  Sem r ScopedIden
checkQualifiedExpr q@(QualifiedName (SymbolPath p) sym) = do
  es <- fst <$> lookupQualifiedSymbol (toList p, sym)
  case es of
    [] -> notInScope
    [e] -> entryToScopedIden q' e
    _ -> throw (ErrAmbiguousSym (AmbiguousSym q' es))
  where
    q' = NameQualified q
    notInScope = throw (ErrQualSymNotInScope (QualSymNotInScope q))

entryToScopedIden :: Members '[InfoTableBuilder] r => Name -> SymbolEntry -> Sem r ScopedIden
entryToScopedIden name e = do
  let scopedName :: S.Name
      scopedName = set S.nameConcrete name (e ^. symbolEntry)
  registerName scopedName
  return (ScopedIden (set S.nameConcrete name (e ^. symbolEntry)))

-- | We gather all symbols which have been defined or marked to be public in the given scope.
exportScope ::
  forall r.
  Members '[State Scope, Error ScoperError] r =>
  Scope ->
  Sem r ExportInfo
exportScope Scope {..} = do
  _exportSymbols <- HashMap.fromList <$> mapMaybeM mkentry (HashMap.toList _scopeSymbols)
  _exportModuleSymbols <- HashMap.fromList <$> mapMaybeM mkentry (HashMap.toList _scopeModuleSymbols)
  return ExportInfo {..}
  where
    mkentry ::
      forall ns.
      SingI ns =>
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
                        SNameSpaceSymbols -> Left es
                        SNameSpaceModules -> Right es
                    )
                )
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
  Members '[Error ScoperError, State Scope, State ScoperState, State ScoperFixities, State ScoperIterators] r =>
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

checkIteratorSyntaxDef ::
  forall r.
  Members '[Error ScoperError, State Scope, State ScoperState, State ScoperFixities, State ScoperIterators] r =>
  IteratorSyntaxDef ->
  Sem r ()
checkIteratorSyntaxDef s@IteratorSyntaxDef {..} = do
  checkNotDefined
  let sf =
        SymbolIterator
          { _symbolIteratorUsed = False,
            _symbolIteratorDef = s
          }
  modify (set (scoperIterators . at _iterSymbol) (Just sf))
  where
    checkNotDefined :: Sem r ()
    checkNotDefined =
      whenJustM
        (HashMap.lookup _iterSymbol <$> gets (^. scoperIterators))
        $ \s' -> throw (ErrDuplicateIterator (DuplicateIterator (s' ^. symbolIteratorDef) s))

-- | Only used as syntactical convenience for registerX functions
(@$>) :: Functor m => (a -> m ()) -> a -> m a
(@$>) f a = f a $> a

checkFunctionDef ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities, State ScoperIterators, Reader BindingStrategy] r =>
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
      names' <- mapM bindVariableSymbol _sigArgNames
      ty' <- checkParseExpressionAtoms _sigArgType
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
    checkClause :: NewFunctionClause 'Parsed -> Sem r (NewFunctionClause 'Scoped)
    checkClause NewFunctionClause {..} = do
      (patterns', body') <- withLocalScope $ do
        p <- mapM checkParsePatternAtom _clausenPatterns
        b <- checkParseExpressionAtoms _clausenBody
        return (p, b)
      return
        NewFunctionClause
          { _clausenBody = body',
            _clausenPatterns = patterns',
            _clausenPipeKw,
            _clausenAssignKw
          }

checkTypeSignature ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities, State ScoperIterators, Reader BindingStrategy] r =>
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
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  InductiveParameters 'Parsed ->
  Sem r (InductiveParameters 'Scoped)
checkInductiveParameters params = do
  _inductiveParametersType <- checkParseExpressionAtoms (params ^. inductiveParametersType)
  _inductiveParametersNames <- mapM bindVariableSymbol (params ^. inductiveParametersNames)
  return InductiveParameters {..}

checkInductiveDef ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities, State ScoperIterators, State ScoperIterators, Reader BindingStrategy] r =>
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
          _nameIterator :: Maybe IteratorAttribs
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

withTopScope :: Members '[State Scope] r => Sem r a -> Sem r a
withTopScope ma = do
  before <- get @Scope
  let scope' =
        ( set scopeLocalSymbols mempty
            . set scopeLocalModuleSymbols mempty
        )
          before
  put scope'
  ma

withLocalScope :: Members '[State Scope] r => Sem r a -> Sem r a
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

fixitiesBlock :: Members '[Error ScoperError] r => Sem (State ScoperFixities ': r) a -> Sem r a
fixitiesBlock m =
  evalState (mempty :: ScoperFixities) $ do
    a <- m
    checkOrphanFixities
    return a

iteratorsBlock :: Members '[Error ScoperError] r => Sem (State ScoperIterators ': r) a -> Sem r a
iteratorsBlock m =
  evalState (mempty :: ScoperIterators) $ do
    a <- m
    checkOrphanIterators
    return a

checkModuleBody ::
  forall r.
  Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r =>
  [Statement 'Parsed] ->
  Sem r (ExportInfo, [Statement 'Scoped])
checkModuleBody body = do
  body' <-
    fmap flattenSections
      . fixitiesBlock
      . iteratorsBlock
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
        goNonDefinitions :: forall t. Members '[Output (Statement s)] t => NonDefinitionsSection s -> Sem t ()
        goNonDefinitions NonDefinitionsSection {..} = do
          mapM_ (output . toStatement) _nonDefinitionsSection
          whenJust _nonDefinitionsNext goDefinitions
          where
            toStatement :: NonDefinition s -> Statement s
            toStatement = \case
              NonDefinitionImport d -> StatementImport d
              NonDefinitionModule d -> StatementModule d
              NonDefinitionFunctionClause d -> StatementFunctionClause d
              NonDefinitionOpenModule d -> StatementOpenModule d

        goDefinitions :: forall t. Members '[Output (Statement s)] t => DefinitionsSection s -> Sem t ()
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
              DefinitionTypeSignature d -> StatementTypeSignature d
              DefinitionProjectionDef d -> StatementProjectionDef d

checkSections ::
  forall r.
  Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities, State ScoperIterators, State ScoperIterators] r =>
  StatementSections 'Parsed ->
  Sem r (StatementSections 'Scoped)
checkSections sec = topBindings $ case sec of
  SectionsEmpty -> return SectionsEmpty
  SectionsDefinitions d -> SectionsDefinitions <$> goDefinitions d
  SectionsNonDefinitions d -> SectionsNonDefinitions <$> goNonDefinitions d
  where
    goNonDefinitions :: NonDefinitionsSection 'Parsed -> Sem (Reader BindingStrategy ': r) (NonDefinitionsSection 'Scoped)
    goNonDefinitions NonDefinitionsSection {..} = do
      sec' <- mapM goNonDefinition _nonDefinitionsSection
      next' <- mapM goDefinitions _nonDefinitionsNext
      return
        NonDefinitionsSection
          { _nonDefinitionsNext = next',
            _nonDefinitionsSection = sec'
          }
      where
        goNonDefinition :: NonDefinition 'Parsed -> Sem (Reader BindingStrategy ': r) (NonDefinition 'Scoped)
        goNonDefinition = \case
          NonDefinitionModule m -> NonDefinitionModule <$> checkLocalModule m
          NonDefinitionImport i -> NonDefinitionImport <$> checkImport i
          NonDefinitionFunctionClause i -> NonDefinitionFunctionClause <$> checkFunctionClause i
          NonDefinitionOpenModule i -> NonDefinitionOpenModule <$> checkOpenModule i

    goDefinitions :: DefinitionsSection 'Parsed -> Sem (Reader BindingStrategy ': r) (DefinitionsSection 'Scoped)
    goDefinitions DefinitionsSection {..} = do
      mapM_ reserveDefinition _definitionsSection
      sec' <- mapM goDefinition _definitionsSection
      next' <- mapM goNonDefinitions _definitionsNext
      return
        DefinitionsSection
          { _definitionsNext = next',
            _definitionsSection = sec'
          }
      where
        reserveDefinition :: Definition 'Parsed -> Sem (Reader BindingStrategy ': r) ()
        reserveDefinition = \case
          DefinitionSyntax s -> void (checkSyntaxDef s)
          DefinitionFunctionDef d -> void (reserveFunctionSymbol d)
          DefinitionTypeSignature d -> void (reserveSymbolOf SKNameFunction Nothing (d ^. sigName))
          DefinitionAxiom d -> void (reserveAxiomSymbol d)
          DefinitionProjectionDef d -> void (reserveProjectionSymbol d)
          DefinitionInductive d -> reserveInductive d
          where
            reserveInductive :: InductiveDef 'Parsed -> Sem (Reader BindingStrategy ': r) ()
            reserveInductive d = do
              void (reserveInductiveSymbol d)
              constrs <- mapM (reserveConstructorSymbol d) (d ^. inductiveConstructors)
              void (defineInductiveModule (head constrs) d)

        goDefinition :: Definition 'Parsed -> Sem (Reader BindingStrategy ': r) (Definition 'Scoped)
        goDefinition = \case
          DefinitionSyntax s -> return (DefinitionSyntax s)
          DefinitionFunctionDef d -> DefinitionFunctionDef <$> checkFunctionDef d
          DefinitionTypeSignature d -> DefinitionTypeSignature <$> checkTypeSignature d
          DefinitionAxiom d -> DefinitionAxiom <$> checkAxiomDef d
          DefinitionInductive d -> DefinitionInductive <$> checkInductiveDef d
          DefinitionProjectionDef d -> DefinitionProjectionDef <$> checkProjectionDef d

        defineInductiveModule :: S.Symbol -> InductiveDef 'Parsed -> Sem (Reader BindingStrategy ': r) (Module 'Parsed 'ModuleLocal)
        defineInductiveModule headConstr i = runReader (getLoc (i ^. inductiveName)) genModule
          where
            genModule :: forall r'. Members '[Reader Interval] r' => Sem r' (Module 'Parsed 'ModuleLocal)
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
                genBody :: Sem r' [Statement 'Parsed]
                genBody = fromMaybe [] <$> runFail genFieldProjections
                  where
                    genFieldProjections :: Sem (Fail ': r') [Statement 'Parsed]
                    genFieldProjections = do
                      fs <- indexFrom 0 . toList <$> getFields
                      return (map (StatementProjectionDef . mkProjection) fs)
                      where
                        mkProjection ::
                          Indexed (RecordField 'Parsed) ->
                          ProjectionDef 'Parsed
                        mkProjection (Indexed idx field) = ProjectionDef {
                            _projectionConstructor = headConstr,
                            _projectionField = field ^. fieldName,
                            _projectionFieldIx = idx
                                               }

                        getFields :: Sem (Fail ': r') (NonEmpty (RecordField 'Parsed))
                        getFields = case i ^. inductiveConstructors of
                          c :| [] -> case c ^. constructorRhs of
                            ConstructorRhsRecord r -> return (r ^. rhsRecordFields)
                            _ -> fail
                          _ -> fail

mkLetSections :: [LetClause 'Parsed] -> StatementSections 'Parsed
mkLetSections = mkSections . map fromLetClause
  where
    fromLetClause :: LetClause 'Parsed -> Statement 'Parsed
    fromLetClause = \case
      LetTypeSig t -> StatementTypeSignature t
      LetFunClause c -> StatementFunctionClause c

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
      StatementTypeSignature t -> Left (DefinitionTypeSignature t)
      StatementFunctionDef n -> Left (DefinitionFunctionDef n)
      StatementInductive i -> Left (DefinitionInductive i)
      StatementSyntax s -> Left (DefinitionSyntax s)
      StatementProjectionDef s -> Left (DefinitionProjectionDef s)
      StatementImport i -> Right (NonDefinitionImport i)
      StatementModule m -> Right (NonDefinitionModule m)
      StatementOpenModule o -> Right (NonDefinitionOpenModule o)
      StatementFunctionClause c -> Right (NonDefinitionFunctionClause c)

reserveLocalModuleSymbol ::
  Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r =>
  Symbol ->
  Sem r S.Symbol
reserveLocalModuleSymbol =
  ignoreFixities . ignoreIterators . reserveSymbolOf SKNameLocalModule Nothing

checkLocalModule ::
  forall r.
  Members '[Error ScoperError, State Scope, Reader ScopeParameters, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r =>
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
  registerName (S.unqualifiedSymbol _modulePath')
  return _moduleRefModule
  where
    inheritScope :: Sem r ()
    inheritScope = do
      absPath <- (S.<.> _modulePath) <$> gets (^. scopePath)
      modify (set scopePath absPath)
      modify (over scopeSymbols (fmap inheritSymbol))
      modify (over scopeModuleSymbols (fmap inheritSymbol))
      where
        inheritSymbol :: forall ns. SingI ns => SymbolInfo ns -> SymbolInfo ns
        inheritSymbol (SymbolInfo s) = SymbolInfo (inheritEntry <$> s)
          where
            inheritEntry :: NameSpaceEntryType ns -> NameSpaceEntryType ns
            inheritEntry =
              over (nsEntry . S.nameWhyInScope) S.BecauseInherited
                . set (nsEntry . S.nameVisibilityAnn) VisPrivate

checkOrphanFixities :: forall r. Members '[Error ScoperError, State ScoperFixities] r => Sem r ()
checkOrphanFixities = do
  declared <- gets (^. scoperFixities)
  let unused = fmap (^. symbolFixityDef) . find (^. symbolFixityUsed . to not) . toList $ declared
  case unused of
    Nothing -> return ()
    Just x -> throw (ErrUnusedOperatorDef (UnusedOperatorDef x))

checkOrphanIterators :: forall r. Members '[Error ScoperError, State ScoperIterators] r => Sem r ()
checkOrphanIterators = do
  declared <- gets (^. scoperIterators)
  let unused = fmap (^. symbolIteratorDef) . find (^. symbolIteratorUsed . to not) . toList $ declared
  case unused of
    Nothing -> return ()
    Just x -> throw (ErrUnusedIteratorDef (UnusedIteratorDef x))

symbolInfoSingle :: SingI ns => NameSpaceEntryType ns -> SymbolInfo ns
symbolInfoSingle p = SymbolInfo $ HashMap.singleton (p ^. nsEntry . S.nameDefinedIn) p

getModuleRef ::
  Members '[State ScoperState] r =>
  ModuleSymbolEntry ->
  Name ->
  Sem r ModuleRef
getModuleRef e n =
  overModuleRef'' (set (moduleRefName . S.nameConcrete) n)
    <$> gets (^?! scoperModules . at (e ^. moduleEntry . S.nameId) . _Just)

lookupModuleSymbol ::
  Members '[Error ScoperError, State Scope, State ScoperState] r =>
  Name ->
  Sem r ModuleRef
lookupModuleSymbol n = do
  es <- snd <$> lookupQualifiedSymbol (path, sym)
  case nonEmpty (resolveShadowing es) of
    Nothing -> notInScope
    Just (x :| []) -> getModuleRef x n
    Just more -> throw (ErrAmbiguousModuleSym (AmbiguousModuleSym n more))
  where
    notInScope = throw (ErrModuleNotInScope (ModuleNotInScope n))
    (path, sym) = case n of
      NameUnqualified s -> ([], s)
      NameQualified (QualifiedName (SymbolPath p) s) -> (toList p, s)

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
      let exportInfo = moduleRef'' ^. moduleExportInfo
      registerName (moduleRef'' ^. moduleRefName)

      let checkUsingHiding :: UsingHiding 'Parsed -> Sem r (UsingHiding 'Scoped)
          checkUsingHiding = \case
            Hiding h -> Hiding <$> checkHidingList h
            Using uh -> Using <$> checkUsingList uh
            where
              scopeSymbol :: forall (ns :: NameSpace). SingI ns => Sing ns -> Symbol -> Sem r S.Symbol
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
                registerName (S.unqualifiedSymbol scopedSym)
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
                mapM_ (registerName . S.unqualifiedSymbol) scopedAs
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
      where
        mergeSymbol :: forall ns. SingI ns => (Symbol, NameSpaceEntryType ns) -> Sem r ()
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
              _exportModuleSymbols = alterEntry <$> nfo ^. exportModuleSymbols
            }

        alterEntry :: SingI ns => NameSpaceEntryType ns -> NameSpaceEntryType ns
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
            where
              inUsing ::
                forall (ns :: NameSpace).
                SingI ns =>
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
            where
              inHiding :: forall ns. SingI ns => NameSpaceEntryType ns -> Bool
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
  | otherwise = checkOpenModuleNoImport op

checkFunctionClause ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, Reader BindingStrategy] r =>
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
        _clauseBody = clauseBody',
        _clauseAssignKw
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
  Members '[Reader ScopeParameters, InfoTableBuilder, Error ScoperError, State Scope, State ScoperState, NameIdGen, State ScoperFixities, State ScoperIterators, Reader BindingStrategy] r =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiomDef AxiomDef {..} = do
  axiomType' <- withLocalScope (checkParseExpressionAtoms _axiomType)
  axiomName' <- bindAxiomSymbol _axiomName
  axiomDoc' <- withLocalScope (mapM checkJudoc _axiomDoc)
  registerAxiom @$> AxiomDef {_axiomName = axiomName', _axiomType = axiomType', _axiomDoc = axiomDoc', ..}

entryToSymbol :: forall (ns :: NameSpace). SingI ns => NameSpaceEntryType ns -> Symbol -> S.Symbol
entryToSymbol sentry csym = set S.nameConcrete csym (sentry ^. nsEntry)

checkFunction ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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
checkLetClauses ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  NonEmpty (LetClause 'Parsed) ->
  Sem r (NonEmpty (LetClause 'Scoped))
checkLetClauses =
  localBindings
    . ignoreFixities
    . ignoreIterators
    . fmap fromSections
    . checkSections
    . mkLetSections
    . toList
  where
    fromSections :: StatementSections s -> NonEmpty (LetClause s)
    fromSections = \case
      SectionsEmpty -> impossible
      SectionsDefinitions d -> fromDefs d
      SectionsNonDefinitions d -> fromNonDefs d
      where
        fromDefs :: DefinitionsSection s -> NonEmpty (LetClause s)
        fromDefs DefinitionsSection {..} =
          (fromDef <$> _definitionsSection) <>? (fromNonDefs <$> _definitionsNext)
          where
            fromDef :: Definition s -> LetClause s
            fromDef = \case
              DefinitionTypeSignature d -> LetTypeSig d
              DefinitionFunctionDef {} -> impossible
              DefinitionInductive {} -> impossible
              DefinitionProjectionDef {} -> impossible
              DefinitionAxiom {} -> impossible
              DefinitionSyntax {} -> impossible
        fromNonDefs :: NonDefinitionsSection s -> NonEmpty (LetClause s)
        fromNonDefs NonDefinitionsSection {..} =
          (fromNonDef <$> _nonDefinitionsSection) <>? (fromDefs <$> _nonDefinitionsNext)
          where
            fromNonDef :: NonDefinition s -> LetClause s
            fromNonDef = \case
              NonDefinitionFunctionClause f -> LetFunClause f
              NonDefinitionImport {} -> impossible
              NonDefinitionModule {} -> impossible
              NonDefinitionOpenModule {} -> impossible

checkListPattern ::
  forall r.
  Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  ListPattern 'Parsed ->
  Sem r (ListPattern 'Scoped)
checkListPattern l = do
  let _listpBracketL = l ^. listpBracketL
      _listpBracketR = l ^. listpBracketR
  _listpItems <- mapM checkParsePatternAtoms (l ^. listpItems)
  return ListPattern {..}

checkList ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  List 'Parsed ->
  Sem r (List 'Scoped)
checkList l = do
  let _listBracketL = l ^. listBracketL
      _listBracketR = l ^. listBracketR
  _listItems <- mapM checkParseExpressionAtoms (l ^. listItems)
  return List {..}

checkLet ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  Let 'Parsed ->
  Sem r (Let 'Scoped)
checkLet Let {..} =
  withLocalScope $ do
    letClauses' <- checkLetClauses _letClauses
    letExpression' <- checkParseExpressionAtoms _letExpression
    return
      Let
        { _letClauses = letClauses',
          _letExpression = letExpression',
          _letKw,
          _letInKw
        }

checkCaseBranch ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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
  RefNameType 'S.NotConcrete ->
  Symbol ->
  Sem r S.Name
scopedFunction fref n = do
  let scoped :: S.Name = set S.nameConcrete (NameUnqualified n) fref
  registerName scoped
  return scoped

checkUnqualified ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  Symbol ->
  Sem r ScopedIden
checkUnqualified s = do
  scope <- get
  -- Lookup at the global scope
  let err = throw (ErrSymNotInScope (NotInScope s scope))
  entries <- fst <$> lookupQualifiedSymbol ([], s)
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
resolveShadowing :: forall ns. SingI ns => [NameSpaceEntryType ns] -> [NameSpaceEntryType ns]
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
  Members '[Error ScoperError, State Scope, NameIdGen, State ScoperState, InfoTableBuilder] r =>
  Name ->
  Sem r PatternScopedIden
checkPatternName n = do
  c <- getConstructorRef
  case c of
    Just constr -> do
      registerName constr
      return (PatternScopedConstructor constr) -- the symbol is a constructor
    Nothing -> PatternScopedVar <$> bindVariableSymbol sym -- the symbol is a variable
  where
    (path, sym) = case n of
      NameQualified (QualifiedName (SymbolPath p) s) -> (toList p, s)
      NameUnqualified s -> ([], s)
    -- check whether the symbol is a constructor in scope
    getConstructorRef :: Sem r (Maybe S.Name)
    getConstructorRef = do
      entries <- mapMaybe getConstructor . fst <$> lookupQualifiedSymbol (path, sym)
      case entries of
        [] -> case SymbolPath <$> nonEmpty path of
          Nothing -> return Nothing -- There is no constructor with such a name
          Just pth -> throw (ErrQualSymNotInScope (QualSymNotInScope (QualifiedName pth sym)))
        [e] -> return (Just (set S.nameConcrete n e)) -- There is one constructor with such a name
        es -> throw (ErrAmbiguousSym (AmbiguousSym n (map SymbolEntry es)))
    getConstructor :: SymbolEntry -> Maybe (S.Name' ())
    getConstructor e = case getNameKind e of
      KNameConstructor -> Just (e ^. symbolEntry)
      _ -> Nothing

checkPatternBinding ::
  Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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
  PatternAtomAt p -> PatternAtomAt <$> checkPatternBinding p
  PatternAtomList l -> PatternAtomList <$> checkListPattern l

checkName ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder] r) =>
  Name ->
  Sem r ScopedIden
checkName n = case n of
  NameQualified q -> checkQualifiedExpr q
  NameUnqualified s -> checkUnqualified s

checkExpressionAtom ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtom 'Parsed ->
  Sem r (NonEmpty (ExpressionAtom 'Scoped))
checkExpressionAtom e = case e of
  AtomIdentifier n -> pure . AtomIdentifier <$> checkName n
  AtomLambda lam -> pure . AtomLambda <$> checkLambda lam
  AtomCase c -> pure . AtomCase <$> checkCase c
  AtomLet letBlock -> pure . AtomLet <$> checkLet letBlock
  AtomUniverse uni -> return (pure (AtomUniverse uni))
  AtomFunction fun -> pure . AtomFunction <$> checkFunction fun
  AtomParens par -> pure . AtomParens <$> checkParens par
  AtomBraces br -> pure . AtomBraces <$> traverseOf withLocParam checkParseExpressionAtoms br
  AtomFunArrow a -> return (pure (AtomFunArrow a))
  AtomHole h -> pure . AtomHole <$> checkHole h
  AtomLiteral l -> return (pure (AtomLiteral l))
  AtomList l -> pure . AtomList <$> checkList l
  AtomIterator i -> pure . AtomIterator <$> checkIterator i
  AtomNamedApplication i -> pure . AtomNamedApplication <$> checkNamedApplication i
  AtomAmbiguousIterator i -> checkAmbiguousIterator i

checkAmbiguousIterator ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  AmbiguousIterator ->
  Sem r (NonEmpty (ExpressionAtom 'Scoped))
checkAmbiguousIterator a = do
  nm <- checkName (a ^. ambiguousIteratorName)
  if
      | isJust (identifierName nm ^. S.nameIterator) -> pure . AtomIterator <$> checkIterator (fromAmbiguousIterator a)
      | otherwise -> (^. expressionAtoms) <$> checkExpressionAtoms (P.ambiguousIteratorToAtoms a)

checkNamedApplication ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  NamedApplication 'Parsed ->
  Sem r (NamedApplication 'Scoped)
checkNamedApplication napp = do
  _namedAppName <- checkName (napp ^. namedAppName)
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

getNameSignature :: Members '[State ScoperState, Error ScoperError] r => ScopedIden -> Sem r NameSignature
getNameSignature s = do
  sig <- maybeM (throw err) return (getNameSignatureMay s)
  when (null (sig ^. nameSignatureArgs)) (throw err)
  return sig
  where
    err = ErrNoNameSignature (NoNameSignature s)

getNameSignatureMay :: Members '[State ScoperState] r => ScopedIden -> Sem r (Maybe NameSignature)
getNameSignatureMay s = gets (^. scoperSignatures . at (identifierName s ^. S.nameId))

checkIterator ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  Iterator 'Parsed ->
  Sem r (Iterator 'Scoped)
checkIterator iter = do
  _iteratorName <- checkName (iter ^. iteratorName)
  case identifierName _iteratorName ^. S.nameIterator of
    Just IteratorAttribs {..} -> do
      case _iteratorAttribsInitNum of
        Just n
          | n /= length (iter ^. iteratorInitializers) ->
              throw
                ( ErrIteratorInitializer
                    IteratorInitializer {_iteratorInitializerIterator = iter}
                )
        _ -> return ()
      case _iteratorAttribsRangeNum of
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
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
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
  Members '[NameIdGen] r =>
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
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParens e@(ExpressionAtoms as _) = case as of
  AtomIdentifier s :| [] -> do
    scopedId <- checkName s
    let scopedIdenNoFix = over scopedIden (set S.nameFixity Nothing) scopedId
    return (ExpressionParensIdentifier scopedIdenNoFix)
  AtomIterator i :| [] -> ExpressionIterator . set iteratorParens True <$> checkIterator i
  AtomCase c :| [] -> ExpressionCase . set caseParens True <$> checkCase c
  _ -> checkParseExpressionAtoms e

checkExpressionAtoms ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtoms 'Parsed ->
  Sem r (ExpressionAtoms 'Scoped)
checkExpressionAtoms (ExpressionAtoms l i) = (`ExpressionAtoms` i) <$> sconcatMap checkExpressionAtom l

checkJudoc ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  Judoc 'Parsed ->
  Sem r (Judoc 'Scoped)
checkJudoc (Judoc groups) = Judoc <$> mapM checkJudocGroup groups

checkJudocGroup ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  JudocGroup 'Parsed ->
  Sem r (JudocGroup 'Scoped)
checkJudocGroup = \case
  JudocGroupBlock b -> JudocGroupBlock <$> checkJudocBlockParagraph b
  JudocGroupLines l -> JudocGroupLines <$> mapM checkJudocBlock l

checkJudocBlock ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  JudocBlock 'Parsed ->
  Sem r (JudocBlock 'Scoped)
checkJudocBlock = \case
  JudocLines l -> JudocLines <$> mapM checkJudocLine l
  JudocExample e -> JudocExample <$> traverseOf exampleExpression checkParseExpressionAtoms e

checkJudocBlockParagraph ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  JudocBlockParagraph 'Parsed ->
  Sem r (JudocBlockParagraph 'Scoped)
checkJudocBlockParagraph = traverseOf judocBlockParagraphBlocks (mapM checkJudocBlock)

checkJudocLine ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  JudocLine 'Parsed ->
  Sem r (JudocLine 'Scoped)
checkJudocLine (JudocLine delim atoms) = JudocLine delim <$> mapM (mapM checkJudocAtom) atoms

checkJudocAtom ::
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  JudocAtom 'Parsed ->
  Sem r (JudocAtom 'Scoped)
checkJudocAtom = \case
  JudocText t -> return (JudocText t)
  JudocExpression e -> JudocExpression <$> checkParseExpressionAtoms e

checkParseExpressionAtoms ::
  forall r.
  Members '[Reader ScopeParameters, Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
checkParseExpressionAtoms = checkExpressionAtoms >=> parseExpressionAtoms

checkParsePatternAtom ::
  (Members '[Error ScoperError, State Scope, State ScoperState, InfoTableBuilder, NameIdGen] r) =>
  PatternAtom 'Parsed ->
  Sem r PatternArg
checkParsePatternAtom = checkPatternAtom >=> parsePatternAtom

checkSyntaxDef ::
  (Members '[Error ScoperError, Reader ScopeParameters, State Scope, State ScoperState, InfoTableBuilder, NameIdGen, State ScoperFixities, State ScoperIterators, State ScoperIterators] r) =>
  SyntaxDef ->
  Sem r SyntaxDef
checkSyntaxDef = \case
  SyntaxOperator opDef -> SyntaxOperator opDef <$ checkOperatorSyntaxDef opDef
  SyntaxIterator iterDef -> SyntaxIterator iterDef <$ checkIteratorSyntaxDef iterDef

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
                  _paramDelims = Irrelevant Nothing,
                  _paramColon = Irrelevant Nothing,
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
      <|> parseList
      <|> parseLiteral
      <|> parseLet
      <|> parseIterator
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
    getConstructorRef :: PatternAtom 'Scoped -> Maybe S.Name
    getConstructorRef = \case
      PatternAtomIden i -> case i of
        PatternScopedConstructor c -> Just c
        _ -> Nothing
      _ -> Nothing
    operators = mkSymbolTable (mapMaybe getConstructorRef (toList latoms))
    mkSymbolTable :: [S.Name] -> [[P.Operator ParsePat PatternArg]]
    mkSymbolTable = reverse . map (map snd) . groupSortOn' fst . mapMaybe unqualifiedSymbolOp
      where
        unqualifiedSymbolOp :: S.Name -> Maybe (Precedence, P.Operator ParsePat PatternArg)
        unqualifiedSymbolOp S.Name' {..}
          | Just Fixity {..} <- _nameFixity,
            _nameKind == KNameConstructor = Just $
              case _fixityArity of
                Unary u -> (_fixityPrecedence, P.Postfix (unaryApp <$> parseSymbolId _nameId))
                  where
                    unaryApp :: S.Name -> PatternArg -> PatternArg
                    unaryApp constrName = case u of
                      AssocPostfix -> explicitP . PatternPostfixApplication . (`PatternPostfixApp` constrName)
                Binary b -> (_fixityPrecedence, infixLRN (binaryInfixApp <$> parseSymbolId _nameId))
                  where
                    binaryInfixApp :: S.Name -> PatternArg -> PatternArg -> PatternArg
                    binaryInfixApp name argLeft = explicitP . PatternInfixApplication . PatternInfixApp argLeft name
                    infixLRN :: ParsePat (PatternArg -> PatternArg -> PatternArg) -> P.Operator ParsePat PatternArg
                    infixLRN = case b of
                      AssocLeft -> P.InfixL
                      AssocRight -> P.InfixR
                      AssocNone -> P.InfixN
          | otherwise = Nothing
        parseSymbolId :: S.NameId -> ParsePat S.Name
        parseSymbolId uid = P.token getConstructorRefWithId mempty
          where
            getConstructorRefWithId :: PatternAtom 'Scoped -> Maybe S.Name
            getConstructorRefWithId s = do
              ref <- getConstructorRef s
              guard (ref ^. S.nameId == uid)
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
      <|> parseParens
      <|> parseBraces
      <|> parseWildcard
      <|> parseEmpty
      <|> parseAt
      <|> parseList
  where
    parseNoInfixConstructor :: ParsePat PatternArg
    parseNoInfixConstructor =
      explicitP . PatternConstructor
        <$> P.token constructorNoFixity mempty
      where
        constructorNoFixity :: PatternAtom 'Scoped -> Maybe S.Name
        constructorNoFixity s = case s of
          PatternAtomIden (PatternScopedConstructor ref)
            | not (S.hasFixity n) -> Just ref
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
    singletonAtom a = PatternAtoms (NonEmpty.singleton a) (Irrelevant (getLoc a))

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
