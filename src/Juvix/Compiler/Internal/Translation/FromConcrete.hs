module Juvix.Compiler.Internal.Translation.FromConcrete
  ( module Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context,
    fromConcrete,
    MCache,
    ConstructorInfos,
    goModuleNoCache,
    fromConcreteExpression,
    fromConcreteImport,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.Scope.Base (ScoperState, scoperScopedConstructorFields, scoperScopedSignatures)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra qualified as Concrete
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Internal.Data.NameDependencyInfo qualified as Internal
import Juvix.Compiler.Internal.Extra (foldExplicitApplication)
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Extra.DependencyBuilder
import Juvix.Compiler.Internal.Language (varFromWildcard)
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.NameKind
import Juvix.Prelude
import Safe (lastMay)

type MCache = Cache Concrete.ModuleIndex Internal.Module

-- | Needed to generate field projections.
type ConstructorInfos = HashMap Internal.ConstructorName ConstructorInfo

fromConcrete ::
  (Members '[Reader EntryPoint, Error JuvixError, Builtins, NameIdGen, Termination] r) =>
  Scoper.ScoperResult ->
  Sem r InternalResult
fromConcrete _resultScoper =
  mapError (JuvixError @ScoperError) $ do
    (modulesCache, _resultModules) <-
      runReader @Pragmas mempty
        . runReader @ExportsTable exportTbl
        . evalState @ConstructorInfos mempty
        . runReader namesSigs
        . runReader constrSigs
        . runCacheEmpty goModuleNoCache
        $ mapM goTopModule ms
    let _resultTable = buildTable _resultModules
        _resultDepInfo = buildDependencyInfo _resultModules exportTbl
        _resultModulesCache = ModulesCache modulesCache
    return InternalResult {..}
  where
    ms = _resultScoper ^. Scoper.resultModules
    exportTbl = _resultScoper ^. Scoper.resultExports
    constrSigs = _resultScoper ^. Scoper.resultScoperState . scoperScopedConstructorFields
    namesSigs = _resultScoper ^. Scoper.resultScoperState . scoperScopedSignatures

-- | `StatementInclude`s are not included in the result
buildMutualBlocks ::
  (Members '[Reader Internal.NameDependencyInfo] r) =>
  [Internal.PreStatement] ->
  Sem r [SCC Internal.PreStatement]
buildMutualBlocks ss = do
  depInfo <- ask
  let scomponents :: [SCC Internal.Name] = buildSCCs depInfo
  return (boolHack (mapMaybe nameToPreStatement scomponents))
  where
    -- If the builtin bool definition is found, it is moved at the front.
    --
    -- This is a hack needed to translate BuiltinStringToNat in
    -- internal-to-core. BuiltinStringToNat is the only function that depends on
    -- Bool implicitly (i.e. without mentioning it in its type). Eventually
    -- BuiltinStringToNat needs to be removed and so this hack.
    boolHack :: [SCC Internal.PreStatement] -> [SCC Internal.PreStatement]
    boolHack s = case popFirstJust isBuiltinBool s of
      (Nothing, _) -> s
      (Just boolDef, rest) -> AcyclicSCC (Internal.PreInductiveDef boolDef) : rest
      where
        isBuiltinBool :: SCC Internal.PreStatement -> Maybe Internal.InductiveDef
        isBuiltinBool = \case
          CyclicSCC [Internal.PreInductiveDef b]
            | Just BuiltinBool <- b ^. Internal.inductiveBuiltin -> Just b
          _ -> Nothing

    statementsByName :: HashMap Internal.Name Internal.PreStatement
    statementsByName = HashMap.fromList (map mkAssoc ss)
      where
        mkAssoc :: Internal.PreStatement -> (Internal.Name, Internal.PreStatement)
        mkAssoc s = case s of
          Internal.PreInductiveDef i -> (i ^. Internal.inductiveName, s)
          Internal.PreFunctionDef i -> (i ^. Internal.funDefName, s)
          Internal.PreAxiomDef i -> (i ^. Internal.axiomName, s)

    getStmt :: Internal.Name -> Maybe Internal.PreStatement
    getStmt n = statementsByName ^. at n

    nameToPreStatement :: SCC Internal.Name -> Maybe (SCC Internal.PreStatement)
    nameToPreStatement = nonEmptySCC . fmap getStmt
      where
        nonEmptySCC :: SCC (Maybe a) -> Maybe (SCC a)
        nonEmptySCC = \case
          AcyclicSCC a -> AcyclicSCC <$> a
          CyclicSCC p -> CyclicSCC . toList <$> nonEmpty (catMaybes p)

buildLetMutualBlocks ::
  NonEmpty Internal.PreLetStatement ->
  NonEmpty (SCC Internal.PreLetStatement)
buildLetMutualBlocks ss = nonEmpty' . mapMaybe nameToPreStatement $ scomponents
  where
    -- TODO buildDependencyInfoLet is repeating too much work when there are big nested lets
    depInfo = buildDependencyInfoLet ss

    scomponents :: [SCC Internal.Name] = buildSCCs depInfo

    statementsByName :: HashMap Internal.Name Internal.PreLetStatement
    statementsByName = HashMap.fromList (map mkAssoc (toList ss))
      where
        mkAssoc :: Internal.PreLetStatement -> (Internal.Name, Internal.PreLetStatement)
        mkAssoc s = case s of
          Internal.PreLetFunctionDef i -> (i ^. Internal.funDefName, s)

    getStmt :: Internal.Name -> Maybe Internal.PreLetStatement
    getStmt n = statementsByName ^. at n

    nameToPreStatement :: SCC Internal.Name -> Maybe (SCC Internal.PreLetStatement)
    nameToPreStatement = nonEmptySCC . fmap getStmt
      where
        nonEmptySCC :: SCC (Maybe a) -> Maybe (SCC a)
        nonEmptySCC = \case
          AcyclicSCC a -> AcyclicSCC <$> a
          CyclicSCC p -> CyclicSCC . toList <$> nonEmpty (catMaybes p)

fromConcreteExpression :: (Members '[Builtins, Error JuvixError, NameIdGen, Termination, State ScoperState] r) => Scoper.Expression -> Sem r Internal.Expression
fromConcreteExpression e = do
  nameSigs <- gets (^. scoperScopedSignatures)
  constrSigs <- gets (^. scoperScopedConstructorFields)
  e' <-
    mapError (JuvixError @ScoperError)
      . runReader @Pragmas mempty
      . runReader nameSigs
      . runReader constrSigs
      . goExpression
      $ e
  checkTerminationShallow e'
  return e'

fromConcreteImport ::
  (Members '[Reader ExportsTable, Error JuvixError, NameIdGen, Builtins, MCache, Termination] r) =>
  Scoper.Import 'Scoped ->
  Sem r Internal.Import
fromConcreteImport i = do
  i' <-
    mapError (JuvixError @ScoperError)
      . runReader @Pragmas mempty
      . goImport
      $ i
  checkTerminationShallow i'
  return i'

goLocalModule ::
  (Members '[Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ConstructorInfos, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  Module 'Scoped 'ModuleLocal ->
  Sem r [Internal.PreStatement]
goLocalModule = concatMapM goAxiomInductive . (^. moduleBody)

goTopModule ::
  (Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, MCache] r) =>
  Module 'Scoped 'ModuleTop ->
  Sem r Internal.Module
goTopModule = cacheGet . ModuleIndex

goModuleNoCache ::
  (Members '[Reader EntryPoint, Reader ExportsTable, Error JuvixError, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, MCache, State ConstructorInfos, Termination, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  ModuleIndex ->
  Sem r Internal.Module
goModuleNoCache (ModuleIndex m) = do
  p <- toPreModule m
  tbl <- ask
  let depInfo = buildDependencyInfoPreModule p tbl
  r <- runReader depInfo (fromPreModule p)
  noTerminationOption <- asks (^. entryPointNoTermination)
  unless noTerminationOption (checkTerminationShallow r)
  return r

goPragmas :: (Member (Reader Pragmas) r) => Maybe ParsedPragmas -> Sem r Pragmas
goPragmas p = do
  p' <- ask
  return $ p' <> p ^. _Just . withLocParam . withSourceValue

goScopedIden :: ScopedIden -> Internal.Name
goScopedIden iden =
  set Internal.namePretty prettyStr (goSymbol (S.nameUnqualify name))
  where
    name :: S.Name
    name = iden ^. scopedIdenFinal
    prettyStr :: Text
    prettyStr = prettyText name

goSymbol :: S.Symbol -> Internal.Name
goSymbol s = goSymbolPretty (S.symbolText s) s

goSymbolPretty :: Text -> S.Symbol -> Internal.Name
goSymbolPretty pp s =
  Internal.Name
    { _nameText = S.symbolText s,
      _nameId = s ^. S.nameId,
      _nameKind = getNameKind s,
      _namePretty = pp,
      _nameLoc = s ^. S.nameConcrete . symbolLoc,
      _nameFixity = s ^. S.nameFixity
    }

-- TODO give a better name?
traverseM' ::
  forall r s t a b.
  (Monad r, Monad s, Traversable t) =>
  (a -> r (s b)) ->
  t a ->
  r (s (t b))
traverseM' f x = sequence <$> traverse f x

toPreModule ::
  forall r t.
  (SingI t, Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, MCache, State ConstructorInfos, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  Module 'Scoped t ->
  Sem r Internal.PreModule
toPreModule Module {..} = do
  pragmas' <- goPragmas _modulePragmas
  body' <- local (const pragmas') (goModuleBody _moduleBody)
  examples' <- goExamples _moduleDoc
  return
    Internal.Module
      { _moduleName = name',
        _moduleBody = body',
        _moduleExamples = examples',
        _modulePragmas = pragmas'
      }
  where
    name' :: Internal.Name
    name' = case sing :: SModuleIsTop t of
      SModuleTop -> goTopModulePath _modulePath
      SModuleLocal -> goSymbol _modulePath

goTopModulePath :: S.TopModulePath -> Internal.Name
goTopModulePath p = goSymbolPretty (prettyText p) (S.topModulePathSymbol p)

fromPreModule ::
  forall r.
  (Members '[Reader Internal.NameDependencyInfo, Error ScoperError, Builtins, NameIdGen, Reader Pragmas] r) =>
  Internal.PreModule ->
  Sem r Internal.Module
fromPreModule = traverseOf Internal.moduleBody fromPreModuleBody

fromPreModuleBody ::
  forall r.
  (Members '[Reader Internal.NameDependencyInfo, Error ScoperError, Builtins, NameIdGen, Reader Pragmas] r) =>
  Internal.PreModuleBody ->
  Sem r Internal.ModuleBody
fromPreModuleBody b = do
  sccs <- buildMutualBlocks (b ^. Internal.moduleStatements)
  let moduleStatements' = map goSCC sccs
  return (set Internal.moduleStatements moduleStatements' b)
  where
    goSCC :: SCC Internal.PreStatement -> Internal.MutualBlock
    goSCC = \case
      AcyclicSCC s -> goAcyclic s
      CyclicSCC c -> goCyclic (nonEmpty' c)
      where
        goCyclic :: NonEmpty Internal.PreStatement -> Internal.MutualBlock
        goCyclic c = Internal.MutualBlock (goMutual <$> c)
          where
            goMutual :: Internal.PreStatement -> Internal.MutualStatement
            goMutual = \case
              Internal.PreInductiveDef i -> Internal.StatementInductive i
              Internal.PreFunctionDef i -> Internal.StatementFunction i
              Internal.PreAxiomDef i -> Internal.StatementAxiom i

        goAcyclic :: Internal.PreStatement -> Internal.MutualBlock
        goAcyclic = \case
          Internal.PreInductiveDef i -> one (Internal.StatementInductive i)
          Internal.PreFunctionDef i -> one (Internal.StatementFunction i)
          Internal.PreAxiomDef i -> one (Internal.StatementAxiom i)
          where
            one :: Internal.MutualStatement -> Internal.MutualBlock
            one = Internal.MutualBlock . pure

goModuleBody ::
  forall r.
  (Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, MCache, State ConstructorInfos, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  [Statement 'Scoped] ->
  Sem r Internal.PreModuleBody
goModuleBody stmts = do
  _moduleImports <- mapM goImport (scanImports stmts)
  otherThanFunctions :: [Indexed Internal.PreStatement] <- concatMapM (traverseM' goAxiomInductive) ss
  functions <- map (fmap Internal.PreFunctionDef) <$> compiledFunctions
  projections <- map (fmap Internal.PreFunctionDef) <$> mkProjections
  let unsorted = otherThanFunctions <> functions <> projections
      _moduleStatements = map (^. indexedThing) (sortOn (^. indexedIx) unsorted)
  return Internal.ModuleBody {..}
  where
    ss' :: [Statement 'Scoped]
    ss' = concatMap Concrete.flattenStatement stmts

    ss :: [Indexed (Statement 'Scoped)]
    ss = zipWith Indexed [0 ..] ss'

    mkProjections :: Sem r [Indexed Internal.FunctionDef]
    mkProjections =
      sequence
        [ Indexed i <$> funDef
          | Indexed i (StatementProjectionDef f) <- ss,
            let funDef = goProjectionDef f
        ]

    compiledFunctions :: Sem r [Indexed Internal.FunctionDef]
    compiledFunctions =
      sequence
        [ Indexed i <$> funDef
          | Indexed i (StatementFunctionDef f) <- ss,
            let funDef = goFunctionDef f
        ]

scanImports :: [Statement 'Scoped] -> [Import 'Scoped]
scanImports = mconcatMap go
  where
    go :: Statement 'Scoped -> [Import 'Scoped]
    go = \case
      StatementImport t -> [t]
      StatementModule m -> concatMap go (m ^. moduleBody)
      StatementOpenModule {} -> []
      StatementInductive {} -> []
      StatementAxiom {} -> []
      StatementSyntax {} -> []
      StatementFunctionDef {} -> []
      StatementProjectionDef {} -> []

goImport ::
  forall r.
  (Members '[Reader ExportsTable, Error ScoperError, Builtins, NameIdGen, Reader Pragmas, MCache] r) =>
  Import 'Scoped ->
  Sem r Internal.Import
goImport Import {..} = do
  let m = _importModule ^. moduleRefModule
  m' <- goTopModule m
  return
    ( Internal.Import
        { _importModule = Internal.ModuleIndex m'
        }
    )

-- | Ignores functions
goAxiomInductive ::
  forall r.
  (Members '[Error ScoperError, Builtins, NameIdGen, Reader Pragmas, State ConstructorInfos, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  Statement 'Scoped ->
  Sem r [Internal.PreStatement]
goAxiomInductive = \case
  StatementInductive i -> pure . Internal.PreInductiveDef <$> goInductive i
  StatementAxiom d -> pure . Internal.PreAxiomDef <$> goAxiom d
  StatementModule f -> goLocalModule f
  StatementImport {} -> return []
  StatementFunctionDef {} -> return []
  StatementSyntax {} -> return []
  StatementOpenModule {} -> return []
  StatementProjectionDef {} -> return []

goProjectionDef ::
  forall r.
  (Members '[NameIdGen, State ConstructorInfos] r) =>
  ProjectionDef 'Scoped ->
  Sem r Internal.FunctionDef
goProjectionDef ProjectionDef {..} = do
  let c = goSymbol _projectionConstructor
  info <- gets @ConstructorInfos (^?! at c . _Just)
  Internal.genFieldProjection (goSymbol _projectionField) info _projectionFieldIx

goFunctionDef ::
  forall r.
  (Members '[Reader Pragmas, Error ScoperError, Builtins, NameIdGen, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  FunctionDef 'Scoped ->
  Sem r Internal.FunctionDef
goFunctionDef FunctionDef {..} = do
  let _funDefName = goSymbol _signName
      _funDefTerminating = isJust _signTerminating
      _funDefInstance = isJust _signInstance
      _funDefBuiltin = (^. withLocParam) <$> _signBuiltin
  _funDefType <- goDefType
  _funDefExamples <- goExamples _signDoc
  _funDefPragmas <- goPragmas _signPragmas
  _funDefBody <- goBody
  msig <- asks @NameSignatures (^. at (_funDefName ^. Internal.nameId))
  _funDefDefaultSignature <- maybe (return mempty) goNameSignature msig
  let fun = Internal.FunctionDef {..}
  whenJust _signBuiltin (registerBuiltinFunction fun . (^. withLocParam))
  return fun
  where
    goNameSignature :: NameSignature 'Scoped -> Sem r Internal.DefaultSignature
    goNameSignature = fmap Internal.DefaultSignature . concatMapM goBlock . (^. nameSignatureArgs)
      where
        goBlock :: NameBlock 'Scoped -> Sem r [Maybe Internal.Expression]
        goBlock blk = do
          let tbl = indexedByInt (^. nameItemIndex) (blk ^. nameBlock)
              mmaxIx = fst <$> IntMap.lookupMax tbl
          case mmaxIx of
            Nothing -> return []
            Just maxIx ->
              execOutputList $ forM_ [0 .. maxIx] $ \idx ->
                case tbl ^. at idx of
                  Nothing -> output (Nothing @Internal.Expression)
                  Just i -> mapM goExpression (i ^? nameItemDefault . _Just . argDefaultValue) >>= output

    goBody :: Sem r Internal.Expression
    goBody = do
      commonPatterns <- concatMapM (fmap toList . argToPattern) _signArgs
      let goClause :: FunctionClause 'Scoped -> Sem r Internal.LambdaClause
          goClause FunctionClause {..} = do
            _lambdaBody <- goExpression _clausenBody
            extraPatterns <- mapM goPatternArg _clausenPatterns
            let _lambdaPatterns = prependList commonPatterns extraPatterns
            return Internal.LambdaClause {..}
      case _signBody of
        SigBodyExpression body -> do
          body' <- goExpression body
          return $ case nonEmpty commonPatterns of
            Nothing -> body'
            Just _lambdaPatterns -> do
              let _lambdaBody = body'
                  _lambdaType :: Maybe Internal.Expression = Nothing
                  _lambdaClauses = pure Internal.LambdaClause {..}
              Internal.ExpressionLambda Internal.Lambda {..}
        SigBodyClauses cls -> do
          _lambdaClauses <- mapM goClause cls
          let _lambdaType :: Maybe Internal.Expression = Nothing
          return (Internal.ExpressionLambda Internal.Lambda {..})

    goDefType :: Sem r Internal.Expression
    goDefType = do
      args <- concatMapM (fmap toList . argToParam) _signArgs
      ret <- maybe freshHole goExpression _signRetType
      return (Internal.foldFunType args ret)
      where
        freshHole :: Sem r Internal.Expression
        freshHole = do
          i <- freshNameId
          let loc = maybe (getLoc _signName) getLoc (lastMay _signArgs)
              h = mkHole loc i
          return $ Internal.ExpressionHole h

        argToParam :: SigArg 'Scoped -> Sem r (NonEmpty Internal.FunctionParameter)
        argToParam a@SigArg {..} = do
          let _paramImplicit = _sigArgImplicit
          _paramType <- case _sigArgType of
            Nothing -> return (Internal.smallUniverseE (getLoc a))
            Just ty -> goExpression ty

          let _paramImpligoExpressioncit = _sigArgImplicit
              noName = Internal.FunctionParameter {_paramName = Nothing, ..}
              mk :: Concrete.Argument 'Scoped -> Internal.FunctionParameter
              mk ma =
                let _paramName =
                      case ma of
                        Concrete.ArgumentSymbol s -> Just (goSymbol s)
                        Concrete.ArgumentWildcard {} -> Nothing
                 in Internal.FunctionParameter {..}

          return . fromMaybe (pure noName) $ nonEmpty (mk <$> _sigArgNames)

    argToPattern :: SigArg 'Scoped -> Sem r (NonEmpty Internal.PatternArg)
    argToPattern arg@SigArg {..} = do
      let _patternArgIsImplicit = _sigArgImplicit
          _patternArgName :: Maybe Internal.Name = Nothing
          noName = goWidlcard (Wildcard (getLoc arg))
          goWidlcard w = do
            _patternArgPattern <- Internal.PatternVariable <$> varFromWildcard w
            return Internal.PatternArg {..}
          mk :: Concrete.Argument 'Scoped -> Sem r Internal.PatternArg
          mk = \case
            Concrete.ArgumentSymbol s ->
              let _patternArgPattern = Internal.PatternVariable (goSymbol s)
               in return Internal.PatternArg {..}
            Concrete.ArgumentWildcard w -> goWidlcard w
      maybe (pure <$> noName) (mapM mk) (nonEmpty _sigArgNames)

goExamples ::
  forall r.
  (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  Maybe (Judoc 'Scoped) ->
  Sem r [Internal.Example]
goExamples = mapM goExample . maybe [] judocExamples
  where
    goExample :: Example 'Scoped -> Sem r Internal.Example
    goExample ex = do
      e' <- goExpression (ex ^. exampleExpression)
      return
        Internal.Example
          { _exampleExpression = e',
            _exampleId = ex ^. exampleId
          }

goInductiveParameters ::
  forall r.
  (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  InductiveParameters 'Scoped ->
  Sem r [Internal.InductiveParameter]
goInductiveParameters params@InductiveParameters {..} = do
  paramType' <- goRhs _inductiveParametersRhs
  case paramType' of
    Internal.ExpressionUniverse {} -> return ()
    Internal.ExpressionHole {} -> return ()
    _ ->
      throw $
        ErrUnsupported
          Unsupported
            { _unsupportedMsg = "only type variables of small types are allowed",
              _unsupportedLoc = getLoc params
            }

  let goInductiveParameter :: S.Symbol -> Internal.InductiveParameter
      goInductiveParameter var =
        Internal.InductiveParameter
          { _inductiveParamName = goSymbol var
          }
  return (map goInductiveParameter (toList _inductiveParametersNames))
  where
    goRhs :: Maybe (InductiveParametersRhs 'Scoped) -> Sem r Internal.Expression
    goRhs = \case
      Nothing -> return (Internal.smallUniverseE (getLoc params))
      Just rhs -> goExpression (rhs ^. inductiveParametersType)

registerBuiltinInductive ::
  (Members '[Error ScoperError, Builtins] r) =>
  Internal.InductiveDef ->
  BuiltinInductive ->
  Sem r ()
registerBuiltinInductive d = \case
  BuiltinNat -> registerNatDef d
  BuiltinBool -> registerBoolDef d
  BuiltinInt -> registerIntDef d
  BuiltinList -> registerListDef d

registerBuiltinFunction ::
  (Members '[Error ScoperError, Builtins, NameIdGen] r) =>
  Internal.FunctionDef ->
  BuiltinFunction ->
  Sem r ()
registerBuiltinFunction d = \case
  BuiltinNatPlus -> registerNatPlus d
  BuiltinNatSub -> registerNatSub d
  BuiltinNatMul -> registerNatMul d
  BuiltinNatUDiv -> registerNatUDiv d
  BuiltinNatDiv -> registerNatDiv d
  BuiltinNatMod -> registerNatMod d
  BuiltinNatLe -> registerNatLe d
  BuiltinNatLt -> registerNatLt d
  BuiltinNatEq -> registerNatEq d
  BuiltinBoolIf -> registerIf d
  BuiltinBoolOr -> registerOr d
  BuiltinBoolAnd -> registerAnd d
  BuiltinIntEq -> registerIntEq d
  BuiltinIntSubNat -> registerIntSubNat d
  BuiltinIntPlus -> registerIntPlus d
  BuiltinIntNegNat -> registerIntNegNat d
  BuiltinIntNeg -> registerIntNeg d
  BuiltinIntMul -> registerIntMul d
  BuiltinIntDiv -> registerIntDiv d
  BuiltinIntMod -> registerIntMod d
  BuiltinIntSub -> registerIntSub d
  BuiltinIntNonNeg -> registerIntNonNeg d
  BuiltinIntLe -> registerIntLe d
  BuiltinIntLt -> registerIntLt d
  BuiltinSeq -> registerSeq d

registerBuiltinAxiom ::
  (Members '[Error ScoperError, Builtins, NameIdGen] r) =>
  Internal.AxiomDef ->
  BuiltinAxiom ->
  Sem r ()
registerBuiltinAxiom d = \case
  BuiltinIO -> registerIO d
  BuiltinIOSequence -> registerIOSequence d
  BuiltinIOReadline -> registerIOReadline d
  BuiltinNatPrint -> registerNatPrint d
  BuiltinNatToString -> registerNatToString d
  BuiltinString -> registerString d
  BuiltinStringPrint -> registerStringPrint d
  BuiltinStringConcat -> registerStringConcat d
  BuiltinStringEq -> registerStringEq d
  BuiltinStringToNat -> registerStringToNat d
  BuiltinBoolPrint -> registerBoolPrint d
  BuiltinTrace -> registerTrace d
  BuiltinFail -> registerFail d
  BuiltinIntToString -> registerIntToString d
  BuiltinIntPrint -> registerIntPrint d

goInductive ::
  (Members '[NameIdGen, Reader Pragmas, Builtins, Error ScoperError, State ConstructorInfos, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  InductiveDef 'Scoped ->
  Sem r Internal.InductiveDef
goInductive ty@InductiveDef {..} = do
  _inductiveParameters' <- concatMapM goInductiveParameters _inductiveParameters
  _inductiveType' <- mapM goExpression _inductiveType
  _inductivePragmas' <- goPragmas _inductivePragmas
  let inductiveName' = goSymbol _inductiveName
      constrRetType = Internal.foldExplicitApplication (Internal.toExpression inductiveName') (map (Internal.ExpressionIden . Internal.IdenVar . (^. Internal.inductiveParamName)) _inductiveParameters')
  _inductiveConstructors' <-
    local (const _inductivePragmas') $
      mapM (goConstructorDef constrRetType) _inductiveConstructors
  _inductiveExamples' <- goExamples _inductiveDoc
  let loc = getLoc _inductiveName
      indDef =
        Internal.InductiveDef
          { _inductiveParameters = _inductiveParameters',
            _inductiveBuiltin = (^. withLocParam) <$> _inductiveBuiltin,
            _inductiveName = inductiveName',
            _inductiveType = fromMaybe (Internal.ExpressionUniverse (SmallUniverse loc)) _inductiveType',
            _inductiveConstructors = toList _inductiveConstructors',
            _inductiveExamples = _inductiveExamples',
            _inductivePragmas = _inductivePragmas',
            _inductivePositive = isJust (ty ^. inductivePositive),
            _inductiveTrait = isJust (ty ^. inductiveTrait)
          }
  whenJust ((^. withLocParam) <$> _inductiveBuiltin) (registerBuiltinInductive indDef)
  registerInductiveConstructors indDef
  return indDef

-- | Registers constructors so we can access them for generating field projections
registerInductiveConstructors :: (Members '[State ConstructorInfos] r) => Internal.InductiveDef -> Sem r ()
registerInductiveConstructors indDef = do
  m <- get
  put (foldr (uncurry HashMap.insert) m (mkConstructorEntries indDef))

goConstructorDef ::
  forall r.
  (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  Internal.Expression ->
  ConstructorDef 'Scoped ->
  Sem r Internal.ConstructorDef
goConstructorDef retTy ConstructorDef {..} = do
  ty' <- goRhs _constructorRhs
  examples' <- goExamples _constructorDoc
  pragmas' <- goPragmas _constructorPragmas
  return
    Internal.ConstructorDef
      { _inductiveConstructorType = ty',
        _inductiveConstructorExamples = examples',
        _inductiveConstructorName = goSymbol _constructorName,
        _inductiveConstructorPragmas = pragmas'
      }
  where
    goAdtType :: Concrete.RhsAdt 'Scoped -> Sem r Internal.Expression
    goAdtType RhsAdt {..} = do
      args <- mapM goArg _rhsAdtArguments
      return (Internal.foldFunType args retTy)
      where
        goArg :: Concrete.Expression -> Sem r Internal.FunctionParameter
        goArg ty = do
          ty' <- goExpression ty
          return
            Internal.FunctionParameter
              { _paramName = Nothing,
                _paramImplicit = Explicit,
                _paramType = ty'
              }

    goRecordType :: Concrete.RhsRecord 'Scoped -> Sem r Internal.Expression
    goRecordType RhsRecord {..} = do
      params <- mapMaybeM goRecordStatement _rhsRecordStatements
      return (Internal.foldFunType params retTy)
      where
        goRecordStatement :: Concrete.RecordStatement 'Scoped -> Sem r (Maybe Internal.FunctionParameter)
        goRecordStatement = \case
          Concrete.RecordStatementOperator {} -> return Nothing
          Concrete.RecordStatementField RecordField {..} -> do
            ty' <- goExpression _fieldType
            return $
              Just
                Internal.FunctionParameter
                  { _paramName = Just (goSymbol _fieldName),
                    _paramImplicit = Explicit,
                    _paramType = ty'
                  }

    goGadtType :: Concrete.RhsGadt 'Scoped -> Sem r Internal.Expression
    goGadtType = goExpression . (^. Concrete.rhsGadtType)

    goRhs :: Concrete.ConstructorRhs 'Scoped -> Sem r Internal.Expression
    goRhs = \case
      ConstructorRhsGadt r -> goGadtType r
      ConstructorRhsRecord r -> goRecordType r
      ConstructorRhsAdt r -> goAdtType r

goLiteral :: LiteralLoc -> Internal.LiteralLoc
goLiteral = fmap go
  where
    go :: Literal -> Internal.Literal
    go = \case
      LitString s -> Internal.LitString s
      LitInteger i -> Internal.LitInteger i

goListPattern :: (Members '[Builtins, Error ScoperError, NameIdGen] r) => Concrete.ListPattern 'Scoped -> Sem r Internal.Pattern
goListPattern l = do
  nil_ <- getBuiltinName loc BuiltinListNil
  cons_ <- getBuiltinName loc BuiltinListCons
  let mkcons :: Internal.Pattern -> Internal.Pattern -> Internal.Pattern
      mkcons a as =
        Internal.PatternConstructorApp
          Internal.ConstructorApp
            { _constrAppConstructor = cons_,
              _constrAppParameters = map mkpat [a, as],
              _constrAppType = Nothing
            }

      mkpat :: Internal.Pattern -> Internal.PatternArg
      mkpat p =
        Internal.PatternArg
          { _patternArgIsImplicit = Explicit,
            _patternArgPattern = p,
            _patternArgName = Nothing
          }

      mknil :: Internal.Pattern
      mknil =
        Internal.PatternConstructorApp
          Internal.ConstructorApp
            { _constrAppConstructor = nil_,
              _constrAppParameters = [],
              _constrAppType = Nothing
            }
  items <- mapM (goPattern . (^. patternArgPattern)) (l ^. Concrete.listpItems)
  return (foldr mkcons mknil items)
  where
    loc = getLoc l

goExpression ::
  forall r.
  (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  Expression ->
  Sem r Internal.Expression
goExpression = \case
  ExpressionIdentifier nt -> return (goIden nt)
  ExpressionParensIdentifier nt -> return (goIden nt)
  ExpressionApplication a -> Internal.ExpressionApplication <$> goApplication a
  ExpressionCase a -> Internal.ExpressionCase <$> goCase a
  ExpressionNewCase a -> Internal.ExpressionCase <$> goNewCase a
  ExpressionInfixApplication ia -> Internal.ExpressionApplication <$> goInfix ia
  ExpressionPostfixApplication pa -> Internal.ExpressionApplication <$> goPostfix pa
  ExpressionLiteral l -> return (Internal.ExpressionLiteral (goLiteral l))
  ExpressionLambda l -> Internal.ExpressionLambda <$> goLambda l
  ExpressionBraces b -> throw (ErrAppLeftImplicit (AppLeftImplicit b))
  ExpressionDoubleBraces b -> throw (ErrDanglingDoubleBrace (DanglingDoubleBrace b))
  ExpressionLet l -> goLet l
  ExpressionList l -> goList l
  ExpressionUniverse uni -> return (Internal.ExpressionUniverse (goUniverse uni))
  ExpressionFunction func -> Internal.ExpressionFunction <$> goFunction func
  ExpressionHole h -> return (Internal.ExpressionHole h)
  ExpressionInstanceHole h -> return (Internal.ExpressionInstanceHole h)
  ExpressionIterator i -> goIterator i
  ExpressionNamedApplication i -> goNamedApplication i
  ExpressionRecordCreation i -> goRecordCreation i
  ExpressionRecordUpdate i -> goRecordUpdateApp i
  ExpressionParensRecordUpdate i -> Internal.ExpressionLambda <$> goRecordUpdate (i ^. parensRecordUpdate)
  where
    goNamedApplication :: Concrete.NamedApplication 'Scoped -> Sem r Internal.Expression
    goNamedApplication w = do
      s <- ask @NameSignatures
      runReader s (runNamedArguments w) >>= goDesugaredNamedApplication

    goDesugaredNamedApplication :: DesugaredNamedApplication -> Sem r Internal.Expression
    goDesugaredNamedApplication a = do
      let fun = goScopedIden (a ^. dnamedAppIdentifier)
          rename :: Internal.Subs =
            HashMap.fromList
              [ (s, Internal.ExpressionIden s') | s <- a ^.. dnamedAppArgs . each . argName . to goSymbol, let s' = Internal.IdenFunction (set Internal.nameKind KNameFunction s)
              ]
          mkAppArg :: Arg -> Internal.ApplicationArg
          mkAppArg arg =
            Internal.ApplicationArg
              { _appArgIsImplicit = arg ^. argImplicit,
                _appArg = Internal.toExpression (goSymbol (arg ^. argName))
              }
          argNames :: NonEmpty Internal.ApplicationArg = mkAppArg <$> a ^. dnamedAppArgs
          app = Internal.foldApplication (Internal.toExpression fun) (toList argNames)
      clauses <- mapM mkClause (a ^. dnamedAppArgs)
      return
        . Internal.substitutionE rename
        $ Internal.ExpressionLet
          Internal.Let
            { _letExpression = app,
              _letClauses = clauses
            }
      where
        mkClause :: Arg -> Sem r Internal.LetClause
        mkClause arg = do
          body' <- goExpression (arg ^. argValue)
          ty <- goExpression (arg ^. argType)
          -- TODO create helper function for simple function definitions
          return $
            Internal.LetFunDef
              Internal.FunctionDef
                { _funDefName = goSymbol (arg ^. argName),
                  _funDefType = ty,
                  _funDefExamples = [],
                  _funDefInstance = False,
                  _funDefPragmas = mempty,
                  _funDefDefaultSignature = mempty,
                  _funDefTerminating = True,
                  _funDefBuiltin = Nothing,
                  _funDefBody = body'
                }

    goRecordCreation :: Concrete.RecordCreation 'Scoped -> Sem r Internal.Expression
    goRecordCreation Concrete.RecordCreation {..} = do
      sig :: RecordNameSignature 'Scoped <- fromJust <$> asks @ConstructorNameSignatures (^. at (_recordCreationConstructor ^. Concrete.scopedIdenName . S.nameId))
      case nonEmpty _recordCreationFields of
        Nothing -> return (Internal.ExpressionIden (Internal.IdenConstructor (goScopedIden _recordCreationConstructor)))
        Just (fields1 :: NonEmpty (RecordDefineField 'Scoped)) -> do
          let getIx fi =
                let sym = Concrete.symbolParsed (fi ^. Concrete.fieldDefineFunDef . Concrete.signName)
                 in sig ^?! recordNames . at sym . _Just . nameItemIndex
              fieldsByIx = nonEmpty' (sortOn getIx (toList fields1))
          cls <- mapM goField fieldsByIx
          let args :: [Internal.Name] = map (^. Internal.funDefName) (toList cls)
              constr = Internal.toExpression (goScopedIden _recordCreationConstructor)
              e = foldExplicitApplication constr (map Internal.toExpression args)
              rename :: Internal.Subs =
                HashMap.fromList
                  [ (s, Internal.toExpression s') | s <- args, let s' = Internal.IdenFunction (set Internal.nameKind KNameFunction s)
                  ]

          return
            . Internal.substitutionE rename
            . Internal.ExpressionLet
            $ Internal.Let
              { _letClauses = Internal.LetFunDef <$> cls,
                _letExpression = e
              }
          where
            goField :: RecordDefineField 'Scoped -> Sem r Internal.FunctionDef
            goField = goFunctionDef . (^. fieldDefineFunDef)

    goRecordUpdate :: Concrete.RecordUpdate 'Scoped -> Sem r Internal.Lambda
    goRecordUpdate r = do
      cl <- mkClause
      return
        Internal.Lambda
          { _lambdaType = Nothing,
            _lambdaClauses = pure cl
          }
      where
        -- fields indexed by field index.
        mkFieldmap :: Sem r (IntMap (RecordUpdateField 'Scoped))
        mkFieldmap = execState mempty $ mapM go (r ^. recordUpdateFields)
          where
            go :: RecordUpdateField 'Scoped -> Sem (State (IntMap (RecordUpdateField 'Scoped)) ': r) ()
            go f = do
              let idx = f ^. fieldUpdateArgIx
              whenM (gets @(IntMap (RecordUpdateField 'Scoped)) (IntMap.member idx)) (throw repeated)
              modify' (IntMap.insert idx f)
              where
                repeated :: ScoperError
                repeated = ErrRepeatedField (RepeatedField (f ^. fieldUpdateName))

        mkArgs :: [Indexed Internal.VarName] -> Sem r [Internal.Expression]
        mkArgs vs = do
          fieldMap <- mkFieldmap
          execOutputList $
            go (uncurry Indexed <$> IntMap.toAscList fieldMap) vs
          where
            go :: [Indexed (RecordUpdateField 'Scoped)] -> [Indexed Internal.VarName] -> Sem (Output Internal.Expression ': r) ()
            go fields = \case
              [] -> return ()
              Indexed idx var : vars' -> case getArg idx of
                Nothing -> do
                  output (Internal.toExpression var)
                  go fields vars'
                Just (arg, fields') -> do
                  val' <- goExpression (arg ^. fieldUpdateValue)
                  output val'
                  go fields' vars'
              where
                getArg :: Int -> Maybe (RecordUpdateField 'Scoped, [Indexed (RecordUpdateField 'Scoped)])
                getArg idx = do
                  Indexed fidx arg :| fs <- nonEmpty fields
                  guard (idx == fidx)
                  return (arg, fs)

        mkClause :: Sem r Internal.LambdaClause
        mkClause = do
          let extra = r ^. recordUpdateExtra . unIrrelevant
              constr = goSymbol (extra ^. recordUpdateExtraConstructor)
              vars = map goSymbol (extra ^. recordUpdateExtraVars)
              patArg = Internal.mkConstructorVarPattern Explicit constr vars
          args <- mkArgs (indexFrom 0 vars)
          return
            Internal.LambdaClause
              { _lambdaPatterns = pure patArg,
                _lambdaBody = foldExplicitApplication (Internal.toExpression constr) args
              }

    goRecordUpdateApp :: Concrete.RecordUpdateApp -> Sem r Internal.Expression
    goRecordUpdateApp r = do
      expr' <- goExpression (r ^. recordAppExpression)
      lam <- Internal.ExpressionLambda <$> goRecordUpdate (r ^. recordAppUpdate)
      return $ foldExplicitApplication lam [expr']

    goList :: Concrete.List 'Scoped -> Sem r Internal.Expression
    goList l = do
      nil_ <- getBuiltinName loc BuiltinListNil
      cons_ <- getBuiltinName loc BuiltinListCons
      items <- mapM goExpression (l ^. Concrete.listItems)
      return (foldr (\a b -> cons_ Internal.@@ a Internal.@@ b) (Internal.toExpression nil_) items)
      where
        loc = getLoc l

    goIden :: Concrete.ScopedIden -> Internal.Expression
    goIden x = Internal.ExpressionIden $ case getNameKind x of
      KNameAxiom -> Internal.IdenAxiom n'
      KNameInductive -> Internal.IdenInductive n'
      KNameLocal -> Internal.IdenVar n'
      KNameFunction -> Internal.IdenFunction n'
      KNameConstructor -> Internal.IdenConstructor n'
      KNameLocalModule -> impossible
      KNameAlias -> impossible
      KNameTopModule -> impossible
      KNameFixity -> impossible
      where
        n' = goScopedIden x

    goLet :: Let 'Scoped -> Sem r Internal.Expression
    goLet l = do
      _letExpression <- goExpression (l ^. letExpression)
      clauses <- goLetFunDefs (l ^. letFunDefs)
      return $ case nonEmpty clauses of
        Just _letClauses -> Internal.ExpressionLet Internal.Let {..}
        Nothing -> _letExpression

    goLetFunDefs :: NonEmpty (LetStatement 'Scoped) -> Sem r [Internal.LetClause]
    goLetFunDefs clauses = fmap goSCC <$> preLetStatements clauses
      where
        preLetStatements :: NonEmpty (LetStatement 'Scoped) -> Sem r [SCC Internal.PreLetStatement]
        preLetStatements cl = do
          pre <- mapMaybeM preLetStatement (toList cl)
          return $ maybe [] (toList . buildLetMutualBlocks) (nonEmpty pre)
          where
            preLetStatement :: LetStatement 'Scoped -> Sem r (Maybe Internal.PreLetStatement)
            preLetStatement = \case
              LetFunctionDef f -> Just . Internal.PreLetFunctionDef <$> goFunctionDef f
              LetAliasDef {} -> return Nothing
              LetOpen {} -> return Nothing

        goSCC :: SCC Internal.PreLetStatement -> Internal.LetClause
        goSCC = \case
          AcyclicSCC (Internal.PreLetFunctionDef f) -> Internal.LetFunDef f
          CyclicSCC fs -> Internal.LetMutualBlock (Internal.MutualBlockLet fs')
            where
              fs' :: NonEmpty Internal.FunctionDef
              fs' = nonEmpty' (map getFun fs)
                where
                  getFun :: Internal.PreLetStatement -> Internal.FunctionDef
                  getFun = \case
                    Internal.PreLetFunctionDef f -> f

    goApplicationArg :: Expression -> Sem r Internal.ApplicationArg
    goApplicationArg arg =
      let (e, i) = case arg of
            ExpressionBraces b -> (b ^. withLocParam, Implicit)
            ExpressionDoubleBraces b -> (b ^. doubleBracesExpression, ImplicitInstance)
            _ -> (arg, Explicit)
       in do
            e' <- goExpression e
            return (Internal.ApplicationArg i e')

    goApplication :: Application -> Sem r Internal.Application
    goApplication (Application l arg) = do
      l' <- goExpression l
      Internal.ApplicationArg {..} <- goApplicationArg arg
      return (Internal.Application l' _appArg _appArgIsImplicit)

    goPostfix :: PostfixApplication -> Sem r Internal.Application
    goPostfix (PostfixApplication l op) = do
      l' <- goExpression l
      let op' = goIden op
      return (Internal.Application op' l' Explicit)

    goInfix :: InfixApplication -> Sem r Internal.Application
    goInfix (InfixApplication l op r) = do
      l' <- goExpression l
      let op' = goIden op
          l'' = Internal.ExpressionApplication (Internal.Application op' l' Explicit)
      r' <- goExpression r
      return (Internal.Application l'' r' Explicit)

    goIterator :: Iterator 'Scoped -> Sem r Internal.Expression
    goIterator Iterator {..} = do
      inipats' <- mapM goPatternArg inipats
      rngpats' <- mapM goPatternArg rngpats
      expr <- goExpression _iteratorBody
      let lam =
            Internal.ExpressionLambda $
              Internal.Lambda
                { _lambdaClauses = Internal.LambdaClause (nonEmpty' (inipats' ++ rngpats')) expr :| [],
                  _lambdaType = Nothing
                }

          fn = goIden _iteratorName
      inivals' <- mapM goExpression inivals
      rngvals' <- mapM goExpression rngvals
      return $ foldl' mkApp fn (lam : inivals' ++ rngvals')
      where
        inipats = map (^. initializerPattern) _iteratorInitializers
        inivals = map (^. initializerExpression) _iteratorInitializers
        rngpats = map (^. rangePattern) _iteratorRanges
        rngvals = map (^. rangeExpression) _iteratorRanges

        mkApp :: Internal.Expression -> Internal.Expression -> Internal.Expression
        mkApp a1 a2 = Internal.ExpressionApplication $ Internal.Application a1 a2 Explicit

goCase :: forall r. (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) => Case 'Scoped -> Sem r Internal.Case
goCase c = do
  _caseExpression <- goExpression (c ^. caseExpression)
  _caseBranches <- mapM goBranch (c ^. caseBranches)
  let _caseParens = c ^. caseParens
      _caseExpressionType :: Maybe Internal.Expression = Nothing
      _caseExpressionWholeType :: Maybe Internal.Expression = Nothing
  return Internal.Case {..}
  where
    goBranch :: CaseBranch 'Scoped -> Sem r Internal.CaseBranch
    goBranch b = do
      _caseBranchPattern <- goPatternArg (b ^. caseBranchPattern)
      _caseBranchExpression <- goExpression (b ^. caseBranchExpression)
      return Internal.CaseBranch {..}

goNewCase :: forall r. (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) => NewCase 'Scoped -> Sem r Internal.Case
goNewCase c = do
  _caseExpression <- goExpression (c ^. newCaseExpression)
  _caseBranches <- mapM goBranch (c ^. newCaseBranches)
  let _caseParens = False
      _caseExpressionType :: Maybe Internal.Expression = Nothing
      _caseExpressionWholeType :: Maybe Internal.Expression = Nothing
  return Internal.Case {..}
  where
    goBranch :: NewCaseBranch 'Scoped -> Sem r Internal.CaseBranch
    goBranch b = do
      _caseBranchPattern <- goPatternArg (b ^. newCaseBranchPattern)
      _caseBranchExpression <- goExpression (b ^. newCaseBranchExpression)
      return Internal.CaseBranch {..}

goLambda :: forall r. (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) => Lambda 'Scoped -> Sem r Internal.Lambda
goLambda l = do
  clauses' <- mapM goClause (l ^. lambdaClauses)
  return
    Internal.Lambda
      { _lambdaClauses = clauses',
        _lambdaType = Nothing
      }
  where
    goClause :: LambdaClause 'Scoped -> Sem r Internal.LambdaClause
    goClause lc = do
      ps' <- mapM goPatternArg (lc ^. lambdaParameters)
      b' <- goExpression (lc ^. lambdaBody)
      return (Internal.LambdaClause ps' b')

goUniverse :: Universe -> SmallUniverse
goUniverse u
  | isSmallUniverse u = SmallUniverse (getLoc u)
  | otherwise = error "only small universe is supported"

goFunction :: (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) => Function 'Scoped -> Sem r Internal.Function
goFunction f = do
  headParam :| tailParams <- goFunctionParameters (f ^. funParameters)
  ret <- goExpression (f ^. funReturn)
  return $
    Internal.Function
      { _functionLeft = headParam,
        _functionRight = foldr (\param acc -> Internal.ExpressionFunction $ Internal.Function param acc) ret tailParams
      }

goFunctionParameters ::
  (Members '[Builtins, NameIdGen, Error ScoperError, Reader Pragmas, Reader NameSignatures, Reader ConstructorNameSignatures] r) =>
  FunctionParameters 'Scoped ->
  Sem r (NonEmpty Internal.FunctionParameter)
goFunctionParameters FunctionParameters {..} = do
  _paramType' <- goExpression _paramType
  let mkParam param =
        Internal.FunctionParameter
          { Internal._paramType = _paramType',
            Internal._paramImplicit = _paramImplicit,
            Internal._paramName = goSymbol <$> param
          }
  return
    . fromMaybe (pure (mkParam Nothing))
    . nonEmpty
    $ mkParam
      . goFunctionParameter
      <$> _paramNames
  where
    goFunctionParameter :: FunctionParameter 'Scoped -> Maybe (SymbolType 'Scoped)
    goFunctionParameter = \case
      FunctionParameterName n -> Just n
      FunctionParameterWildcard {} -> Nothing

mkConstructorApp :: Internal.ConstrName -> [Internal.PatternArg] -> Internal.ConstructorApp
mkConstructorApp a b = Internal.ConstructorApp a b Nothing

goPatternApplication ::
  (Members '[Builtins, NameIdGen, Error ScoperError] r) =>
  PatternApp ->
  Sem r Internal.ConstructorApp
goPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternApplication a)

goWildcardConstructor ::
  WildcardConstructor 'Scoped ->
  Internal.WildcardConstructor
goWildcardConstructor a = Internal.WildcardConstructor (goScopedIden (a ^. wildcardConstructor))

goPatternConstructor ::
  (Members '[Builtins, NameIdGen, Error ScoperError] r) =>
  ScopedIden ->
  Sem r Internal.ConstructorApp
goPatternConstructor a = uncurry mkConstructorApp <$> viewApp (PatternConstructor a)

goInfixPatternApplication ::
  (Members '[Builtins, NameIdGen, Error ScoperError] r) =>
  PatternInfixApp ->
  Sem r Internal.ConstructorApp
goInfixPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternInfixApplication a)

goPostfixPatternApplication ::
  (Members '[Builtins, NameIdGen, Error ScoperError] r) =>
  PatternPostfixApp ->
  Sem r Internal.ConstructorApp
goPostfixPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternPostfixApplication a)

viewApp :: forall r. (Members '[Builtins, NameIdGen, Error ScoperError] r) => Pattern -> Sem r (Internal.ConstrName, [Internal.PatternArg])
viewApp p = case p of
  PatternConstructor c -> return (goScopedIden c, [])
  PatternWildcardConstructor c -> return (goScopedIden (c ^. wildcardConstructor), [])
  PatternApplication app@(PatternApp _ r) -> do
    r' <- goPatternArg r
    second (`snoc` r') <$> viewAppLeft app
  PatternInfixApplication (PatternInfixApp l c r) -> do
    l' <- goPatternArg l
    r' <- goPatternArg r
    return (goScopedIden c, [l', r'])
  PatternPostfixApplication (PatternPostfixApp l c) -> do
    l' <- goPatternArg l
    return (goScopedIden c, [l'])
  PatternVariable {} -> err
  PatternRecord {} -> err
  PatternWildcard {} -> err
  PatternEmpty {} -> err
  PatternList {} -> err
  where
    viewAppLeft :: PatternApp -> Sem r (Internal.ConstrName, [Internal.PatternArg])
    viewAppLeft app@(PatternApp l _)
      | Implicit <- l ^. patternArgIsImplicit = throw (ErrImplicitPatternLeftApplication (ImplicitPatternLeftApplication app))
      | otherwise = viewApp (l ^. patternArgPattern)
    err = throw (ErrConstructorExpectedLeftApplication (ConstructorExpectedLeftApplication p))

goPatternArg :: (Members '[Builtins, NameIdGen, Error ScoperError] r) => PatternArg -> Sem r Internal.PatternArg
goPatternArg p = do
  pat' <- goPattern (p ^. patternArgPattern)
  return
    Internal.PatternArg
      { _patternArgIsImplicit = p ^. patternArgIsImplicit,
        _patternArgName = goSymbol <$> p ^. patternArgName,
        _patternArgPattern = pat'
      }

goPattern :: (Members '[Builtins, NameIdGen, Error ScoperError] r) => Pattern -> Sem r Internal.Pattern
goPattern p = case p of
  PatternVariable a -> return $ Internal.PatternVariable (goSymbol a)
  PatternList a -> goListPattern a
  PatternConstructor c -> Internal.PatternConstructorApp <$> goPatternConstructor c
  PatternWildcardConstructor c -> return (Internal.PatternWildcardConstructor (goWildcardConstructor c))
  PatternApplication a -> Internal.PatternConstructorApp <$> goPatternApplication a
  PatternInfixApplication a -> Internal.PatternConstructorApp <$> goInfixPatternApplication a
  PatternPostfixApplication a -> Internal.PatternConstructorApp <$> goPostfixPatternApplication a
  PatternWildcard i -> Internal.PatternVariable <$> varFromWildcard i
  PatternRecord i -> goRecordPattern i
  PatternEmpty {} -> error "unsupported empty pattern"

goRecordPattern :: forall r. (Members '[NameIdGen, Error ScoperError, Builtins] r) => RecordPattern 'Scoped -> Sem r Internal.Pattern
goRecordPattern r = do
  let constr = goScopedIden (r ^. recordPatternConstructor)
  params' <- mkPatterns
  return
    ( Internal.PatternConstructorApp
        Internal.ConstructorApp
          { _constrAppConstructor = constr,
            _constrAppType = Nothing,
            _constrAppParameters = params'
          }
    )
  where
    itemField :: RecordPatternItem 'Scoped -> Symbol
    itemField = \case
      RecordPatternItemAssign a -> a ^. recordPatternAssignField
      RecordPatternItemFieldPun a -> a ^. fieldPunField . S.nameConcrete

    goPatternItem :: RecordPatternItem 'Scoped -> Sem r (Int, Internal.PatternArg)
    goPatternItem = \case
      RecordPatternItemAssign a -> do
        arg' <- goPatternArg (a ^. recordPatternAssignPattern)
        return (a ^. recordPatternAssignFieldIx, arg')
      RecordPatternItemFieldPun f -> return (f ^. fieldPunIx, arg)
        where
          arg =
            Internal.PatternArg
              { _patternArgIsImplicit = Explicit,
                _patternArgName = Nothing,
                _patternArgPattern = Internal.PatternVariable (goSymbol (f ^. fieldPunField))
              }

    byIndex :: Sem r (IntMap Internal.PatternArg)
    byIndex = execState mempty (mapM_ go (r ^. recordPatternItems))
      where
        go :: RecordPatternItem 'Scoped -> Sem (State (IntMap Internal.PatternArg) ': r) ()
        go i = do
          (idx, arg) <- raise (goPatternItem i)
          whenM (gets @(IntMap Internal.PatternArg) (IntMap.member idx)) (throw (repeatedField (itemField i)))
          modify' (IntMap.insert idx arg)

    repeatedField :: Symbol -> ScoperError
    repeatedField = ErrRepeatedField . RepeatedField

    mkPatterns :: Sem r [Internal.PatternArg]
    mkPatterns = do
      args <- IntMap.toAscList <$> byIndex
      execOutputList (go 0 args)
      where
        loc = getLoc r
        maxIdx :: Int
        maxIdx = length (r ^. recordPatternSignature . unIrrelevant . recordNames) - 1
        go :: Int -> [(Int, Internal.PatternArg)] -> Sem (Output Internal.PatternArg ': r) ()
        go idx args
          | idx > maxIdx = return ()
          | (ix', arg') : args' <- args,
            ix' == idx = do
              output arg'
              go (idx + 1) args'
          | otherwise = do
              v <- Internal.freshVar loc ("x" <> show idx)
              output (Internal.patternArgFromVar Internal.Explicit v)

goAxiom :: (Members '[Reader Pragmas, Error ScoperError, Builtins, NameIdGen, Reader NameSignatures, Reader ConstructorNameSignatures] r) => AxiomDef 'Scoped -> Sem r Internal.AxiomDef
goAxiom a = do
  _axiomType' <- goExpression (a ^. axiomType)
  _axiomPragmas' <- goPragmas (a ^. axiomPragmas)
  let axiom =
        Internal.AxiomDef
          { _axiomType = _axiomType',
            _axiomBuiltin = (^. withLocParam) <$> a ^. axiomBuiltin,
            _axiomName = goSymbol (a ^. axiomName),
            _axiomPragmas = _axiomPragmas'
          }
  whenJust (a ^. axiomBuiltin) (registerBuiltinAxiom axiom . (^. withLocParam))
  return axiom
