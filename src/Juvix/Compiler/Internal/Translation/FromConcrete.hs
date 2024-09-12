module Juvix.Compiler.Internal.Translation.FromConcrete
  ( module Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context,
    fromConcrete,
    ConstructorInfos,
    DefaultArgsStack,
    goTopModule,
    fromConcreteExpression,
    fromConcreteImport,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Builtins
import Juvix.Compiler.Builtins.Assert
import Juvix.Compiler.Builtins.Pair
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra qualified as Concrete
import Juvix.Compiler.Concrete.Gen qualified as Gen
import Juvix.Compiler.Concrete.Language qualified as Concrete
import Juvix.Compiler.Concrete.Pretty
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Data.NameDependencyInfo qualified as Internal
import Juvix.Compiler.Internal.Extra (mkLetClauses)
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Extra.DependencyBuilder
import Juvix.Compiler.Internal.Language (varFromWildcard)
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Scoped.Data.InfoTable qualified as S
import Juvix.Compiler.Store.Scoped.Language (createExportsTable)
import Juvix.Compiler.Store.Scoped.Language qualified as S
import Juvix.Prelude
import Safe (lastMay)

-- | Needed only to generate field projections.
newtype ConstructorInfos = ConstructorInfos
  { _constructorInfos :: HashMap Internal.ConstructorName ConstructorInfo
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''ConstructorInfos

-- | Needed to detect looping while inserting default arguments
newtype DefaultArgsStack = DefaultArgsStack
  { _defaultArgsStack :: [S.Symbol]
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''DefaultArgsStack

fromConcrete ::
  (Members '[Reader EntryPoint, Error JuvixError, Reader Store.ModuleTable, NameIdGen, Termination] r) =>
  Scoper.ScoperResult ->
  Sem r InternalResult
fromConcrete _resultScoper = do
  mtab <- ask
  let ms = HashMap.elems (mtab ^. Store.moduleTable)
      exportTbl =
        _resultScoper ^. Scoper.resultExports
          <> mconcatMap (createExportsTable . (^. Store.moduleInfoScopedModule . S.scopedModuleExportInfo)) ms
      tab :: S.InfoTable =
        S.getCombinedInfoTable (_resultScoper ^. Scoper.resultScopedModule)
          <> mconcatMap (S.getCombinedInfoTable . (^. Store.moduleInfoScopedModule)) ms
  _resultModule <-
    runReader @Pragmas mempty
      . runReader @ExportsTable exportTbl
      . runReader tab
      . mapError (JuvixError @ScoperError)
      . evalState @ConstructorInfos mempty
      . runReader @DefaultArgsStack mempty
      $ goTopModule m
  return InternalResult {..}
  where
    m = _resultScoper ^. Scoper.resultModule

fromConcreteExpression :: (Members '[Error JuvixError, NameIdGen, Termination, Reader S.InfoTable] r) => Scoper.Expression -> Sem r Internal.Expression
fromConcreteExpression e = do
  e' <-
    mapError (JuvixError @ScoperError)
      . runReader @Pragmas mempty
      . runReader @DefaultArgsStack mempty
      . goExpression
      $ e
  checkTerminationShallow e'
  return e'

fromConcreteImport ::
  (Members '[Reader ExportsTable, Error JuvixError, NameIdGen, Termination] r) =>
  Scoper.Import 'Scoped ->
  Sem r Internal.Import
fromConcreteImport i = do
  mapError (JuvixError @ScoperError)
    . runReader @Pragmas mempty
    . goImport
    $ i

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

goLocalModule ::
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, Error ScoperError, NameIdGen, Reader Pragmas, State ConstructorInfos, Reader S.InfoTable] r) =>
  Module 'Scoped 'ModuleLocal ->
  Sem r [Internal.PreStatement]
goLocalModule = concatMapM goAxiomInductive . (^. moduleBody)

goTopModule ::
  (Members '[Reader DefaultArgsStack, Reader EntryPoint, Reader ExportsTable, Error JuvixError, Error ScoperError, NameIdGen, Reader Pragmas, State ConstructorInfos, Termination, Reader S.InfoTable] r) =>
  Module 'Scoped 'ModuleTop ->
  Sem r Internal.Module
goTopModule m = do
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
goScopedIden iden = goName (iden ^. scopedIdenFinal)

goName :: S.Name -> Internal.Name
goName name =
  set Internal.namePretty prettyStr (goSymbol (S.nameUnqualify name))
  where
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
      _nameKindPretty = getNameKindPretty s,
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
  forall r.
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, Reader ExportsTable, Error ScoperError, NameIdGen, Reader Pragmas, State ConstructorInfos, Reader S.InfoTable] r) =>
  Module 'Scoped 'ModuleTop ->
  Sem r Internal.PreModule
toPreModule Module {..} = do
  pragmas' <- goPragmas _modulePragmas
  body' <- local (const pragmas') (goModuleBody _moduleBody)
  let name' = goTopModulePath _modulePath
  return
    Internal.Module
      { _moduleName = name',
        _moduleBody = body',
        _modulePragmas = pragmas',
        _moduleId
      }

goTopModulePath :: S.TopModulePath -> Internal.Name
goTopModulePath p = goSymbolPretty (prettyText p) (S.topModulePathSymbol p)

fromPreModule ::
  forall r.
  (Members '[Reader Internal.NameDependencyInfo, Error ScoperError, NameIdGen, Reader Pragmas] r) =>
  Internal.PreModule ->
  Sem r Internal.Module
fromPreModule = traverseOf Internal.moduleBody fromPreModuleBody

fromPreModuleBody ::
  forall r.
  (Members '[Reader Internal.NameDependencyInfo, Error ScoperError, NameIdGen, Reader Pragmas] r) =>
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
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, Reader ExportsTable, Error ScoperError, NameIdGen, Reader Pragmas, State ConstructorInfos, Reader S.InfoTable] r) =>
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
  Import 'Scoped ->
  Sem r Internal.Import
goImport Import {..} =
  return
    ( Internal.Import
        { _importModuleName = goName (S.topModulePathName _importModulePath)
        }
    )

-- | Ignores functions
goAxiomInductive ::
  forall r.
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, Error ScoperError, NameIdGen, Reader Pragmas, State ConstructorInfos, Reader S.InfoTable] r) =>
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
  (Members '[NameIdGen, Error ScoperError, State ConstructorInfos, Reader S.InfoTable] r) =>
  ProjectionDef 'Scoped ->
  Sem r Internal.FunctionDef
goProjectionDef ProjectionDef {..} = do
  let c = goSymbol _projectionConstructor
  info <- gets (^?! constructorInfos . at c . _Just)
  fun <-
    Internal.genFieldProjection
      _projectionKind
      (goSymbol _projectionField)
      ( (^. withLocParam)
          <$> _projectionFieldBuiltin
      )
      (fmap (^. withLocParam . withSourceValue) _projectionPragmas)
      info
      _projectionFieldIx
  whenJust (fun ^. Internal.funDefBuiltin) (checkBuiltinFunction fun)
  return fun

goFunctionDef ::
  forall r.
  (Members '[Reader DefaultArgsStack, Reader Pragmas, Error ScoperError, NameIdGen, Reader S.InfoTable] r) =>
  FunctionDef 'Scoped ->
  Sem r Internal.FunctionDef
goFunctionDef FunctionDef {..} = do
  let _funDefName = goSymbol _signName
      _funDefTerminating = isJust _signTerminating
      _funDefIsInstanceCoercion
        | isJust _signCoercion = Just Internal.IsInstanceCoercionCoercion
        | isJust _signInstance = Just Internal.IsInstanceCoercionInstance
        | otherwise = Nothing
      _funDefCoercion = isJust _signCoercion
      _funDefBuiltin = (^. withLocParam) <$> _signBuiltin
  _funDefType <- goDefType
  _funDefPragmas <- goPragmas _signPragmas
  _funDefBody <- goBody
  msig <- asks (^. S.infoNameSigs . at (_funDefName ^. Internal.nameId))
  _funDefArgsInfo <- maybe (return mempty) goNameSignature msig
  let _funDefDocComment = fmap ppPrintJudoc _signDoc
      fun = Internal.FunctionDef {..}
  whenJust _signBuiltin (checkBuiltinFunction fun . (^. withLocParam))
  return fun
  where
    goNameSignature :: NameSignature 'Scoped -> Sem r [Internal.ArgInfo]
    goNameSignature = concatMapM goBlock . (^. nameSignatureArgs)
      where
        goBlock :: NameBlock 'Scoped -> Sem r [Internal.ArgInfo]
        goBlock blk = do
          let tbl = indexedByInt (^. nameItemIndex) (blk ^. nameBlock)
              mmaxIx :: Maybe Int = fst <$> IntMap.lookupMax tbl
          execOutputList $ case mmaxIx of
            Nothing -> output Internal.emptyArgInfo
            Just maxIx ->
              forM_ [0 .. maxIx] $ \idx ->
                case tbl ^. at idx of
                  Nothing -> output Internal.emptyArgInfo
                  Just i -> do
                    _argInfoDefault' <- mapM goExpression (i ^? nameItemDefault . _Just . argDefaultValue)
                    output
                      Internal.ArgInfo
                        { _argInfoDefault = _argInfoDefault',
                          _argInfoName = Just (goSymbol (i ^. nameItemSymbol))
                        }

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

              arguments :: Maybe (NonEmpty (Argument 'Scoped))
              arguments = case _sigArgNames of
                SigArgNamesInstance -> Nothing
                SigArgNames ns -> Just ns

          return (maybe (pure noName) (fmap mk) arguments)

    argToPattern :: SigArg 'Scoped -> Sem r (NonEmpty Internal.PatternArg)
    argToPattern arg@SigArg {..} = do
      let _patternArgIsImplicit = _sigArgImplicit
          _patternArgName :: Maybe Internal.Name = Nothing
          noName = goWildcard (Wildcard (getLoc arg))
          goWildcard w = do
            _patternArgPattern <- Internal.PatternVariable <$> varFromWildcard w
            return Internal.PatternArg {..}
          mk :: Concrete.Argument 'Scoped -> Sem r Internal.PatternArg
          mk = \case
            Concrete.ArgumentSymbol s ->
              let _patternArgPattern = Internal.PatternVariable (goSymbol s)
               in return Internal.PatternArg {..}
            Concrete.ArgumentWildcard w -> goWildcard w

          arguments :: Maybe (NonEmpty (Argument 'Scoped))
          arguments = case _sigArgNames of
            SigArgNamesInstance -> Nothing
            SigArgNames ns -> Just ns
      maybe (pure <$> noName) (mapM mk) arguments

goInductiveParameters ::
  forall r.
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  InductiveParameters 'Scoped ->
  Sem r [Internal.InductiveParameter]
goInductiveParameters params@InductiveParameters {..} = do
  paramType' <- goRhs _inductiveParametersRhs
  let goInductiveParameter :: S.Symbol -> Internal.InductiveParameter
      goInductiveParameter var =
        Internal.InductiveParameter
          { _inductiveParamName = goSymbol var,
            _inductiveParamType = paramType'
          }
  return (map goInductiveParameter (toList _inductiveParametersNames))
  where
    goRhs :: Maybe (InductiveParametersRhs 'Scoped) -> Sem r Internal.Expression
    goRhs = \case
      Nothing -> return (Internal.smallUniverseE (getLoc params))
      Just rhs -> goExpression (rhs ^. inductiveParametersType)

checkBuiltinInductive ::
  (Members '[Error ScoperError, Reader S.InfoTable] r) =>
  Internal.InductiveDef ->
  BuiltinInductive ->
  Sem r ()
checkBuiltinInductive d b = localBuiltins $ case b of
  BuiltinNat -> checkNatDef d
  BuiltinBool -> checkBoolDef d
  BuiltinInt -> checkIntDef d
  BuiltinList -> checkListDef d
  BuiltinMaybe -> checkMaybeDef d
  BuiltinPair -> checkPairDef d
  BuiltinPoseidonState -> checkPoseidonStateDef d
  BuiltinEcPoint -> checkEcPointDef d

localBuiltins :: (Members '[Reader S.InfoTable] r) => Sem (Reader BuiltinsTable ': r) a -> Sem r a
localBuiltins m = do
  t <- asks (^. S.infoBuiltins)
  runReader t m

checkBuiltinFunction ::
  (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) =>
  Internal.FunctionDef ->
  BuiltinFunction ->
  Sem r ()
checkBuiltinFunction d f = localBuiltins $ case f of
  BuiltinAssert -> checkAssert d
  BuiltinNatPlus -> checkNatPlus d
  BuiltinNatSub -> checkNatSub d
  BuiltinNatMul -> checkNatMul d
  BuiltinNatUDiv -> checkNatUDiv d
  BuiltinNatDiv -> checkNatDiv d
  BuiltinNatMod -> checkNatMod d
  BuiltinNatLe -> checkNatLe d
  BuiltinNatLt -> checkNatLt d
  BuiltinNatEq -> checkNatEq d
  BuiltinBoolIf -> checkIf d
  BuiltinBoolOr -> checkOr d
  BuiltinBoolAnd -> checkAnd d
  BuiltinIntEq -> checkIntEq d
  BuiltinIntSubNat -> checkIntSubNat d
  BuiltinIntPlus -> checkIntPlus d
  BuiltinIntNegNat -> checkIntNegNat d
  BuiltinIntNeg -> checkIntNeg d
  BuiltinIntMul -> checkIntMul d
  BuiltinIntDiv -> checkIntDiv d
  BuiltinIntMod -> checkIntMod d
  BuiltinIntSub -> checkIntSub d
  BuiltinIntNonNeg -> checkIntNonNeg d
  BuiltinIntLe -> checkIntLe d
  BuiltinIntLt -> checkIntLt d
  BuiltinFromNat -> checkFromNat d
  BuiltinFromInt -> checkFromInt d
  BuiltinSeq -> checkSeq d
  BuiltinMonadBind -> checkMonadBind d

checkBuiltinAxiom ::
  (Members '[Error ScoperError, NameIdGen, Reader S.InfoTable] r) =>
  Internal.AxiomDef ->
  BuiltinAxiom ->
  Sem r ()
checkBuiltinAxiom d b = localBuiltins $ case b of
  BuiltinIO -> checkIO d
  BuiltinIOSequence -> checkIOSequence d
  BuiltinIOReadline -> checkIOReadline d
  BuiltinNatPrint -> checkNatPrint d
  BuiltinNatToString -> checkNatToString d
  BuiltinString -> checkString d
  BuiltinStringPrint -> checkStringPrint d
  BuiltinStringConcat -> checkStringConcat d
  BuiltinStringEq -> checkStringEq d
  BuiltinStringToNat -> checkStringToNat d
  BuiltinField -> checkField d
  BuiltinFieldEq -> checkFieldEq d
  BuiltinFieldAdd -> checkFieldAdd d
  BuiltinFieldSub -> checkFieldSub d
  BuiltinFieldMul -> checkFieldMul d
  BuiltinFieldDiv -> checkFieldDiv d
  BuiltinFieldFromInt -> checkFieldFromInt d
  BuiltinFieldToNat -> checkFieldToNat d
  BuiltinBoolPrint -> checkBoolPrint d
  BuiltinTrace -> checkTrace d
  BuiltinFail -> checkFail d
  BuiltinIntToString -> checkIntToString d
  BuiltinIntPrint -> checkIntPrint d
  BuiltinAnomaGet -> checkAnomaGet d
  BuiltinAnomaEncode -> checkAnomaEncode d
  BuiltinAnomaDecode -> checkAnomaDecode d
  BuiltinAnomaVerifyDetached -> checkAnomaVerifyDetached d
  BuiltinAnomaSign -> checkAnomaSign d
  BuiltinAnomaSignDetached -> checkAnomaSignDetached d
  BuiltinAnomaVerifyWithMessage -> checkAnomaVerifyWithMessage d
  BuiltinAnomaByteArrayFromAnomaContents -> checkAnomaByteArrayFromAnomaContents d
  BuiltinAnomaByteArrayToAnomaContents -> checkAnomaByteArrayToAnomaContents d
  BuiltinPoseidon -> checkPoseidon d
  BuiltinEcOp -> checkEcOp d
  BuiltinRandomEcPoint -> checkRandomEcPoint d
  BuiltinByte -> checkByte d
  BuiltinByteEq -> checkByteEq d
  BuiltinByteToNat -> checkByteToNat d
  BuiltinByteFromNat -> checkByteFromNat d
  BuiltinByteArray -> checkByteArray d
  BuiltinByteArrayFromListByte -> checkByteArrayFromListByte d
  BuiltinByteArrayLength -> checkByteArrayLength d

goInductive ::
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, NameIdGen, Reader Pragmas, Error ScoperError, State ConstructorInfos, Reader S.InfoTable] r) =>
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
  let loc = getLoc _inductiveName
      indDef =
        Internal.InductiveDef
          { _inductiveParameters = _inductiveParameters',
            _inductiveBuiltin = (^. withLocParam) <$> _inductiveBuiltin,
            _inductiveName = inductiveName',
            _inductiveType = fromMaybe (Internal.ExpressionUniverse (SmallUniverse loc)) _inductiveType',
            _inductiveConstructors = toList _inductiveConstructors',
            _inductivePragmas = _inductivePragmas',
            _inductivePositive = isJust (ty ^. inductivePositive),
            _inductiveTrait = isJust (ty ^. inductiveTrait),
            _inductiveDocComment = fmap ppPrintJudoc _inductiveDoc
          }
  whenJust ((^. withLocParam) <$> _inductiveBuiltin) (checkBuiltinInductive indDef)
  checkInductiveConstructors indDef
  return indDef

-- | Checks constructors so we can access them for generating field projections
checkInductiveConstructors :: (Members '[State ConstructorInfos] r) => Internal.InductiveDef -> Sem r ()
checkInductiveConstructors indDef = do
  m <- gets (^. constructorInfos)
  put (ConstructorInfos $ foldr (uncurry HashMap.insert) m (mkConstructorEntries indDef))

goConstructorDef ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  Internal.Expression ->
  ConstructorDef 'Scoped ->
  Sem r Internal.ConstructorDef
goConstructorDef retTy ConstructorDef {..} = do
  ty' <- goRhs _constructorRhs
  pragmas' <- goPragmas _constructorPragmas
  return
    Internal.ConstructorDef
      { _inductiveConstructorType = ty',
        _inductiveConstructorName = goSymbol _constructorName,
        _inductiveConstructorIsRecord = isRhsRecord _constructorRhs,
        _inductiveConstructorPragmas = pragmas',
        _inductiveConstructorDocComment = fmap ppPrintJudoc _constructorDoc
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
          Concrete.RecordStatementSyntax {} -> return Nothing
          Concrete.RecordStatementField RecordField {..} -> do
            ty' <- goExpression _fieldType
            return $
              Just
                Internal.FunctionParameter
                  { _paramName = Just (goSymbol _fieldName),
                    _paramImplicit = fromIsImplicitField _fieldIsImplicit,
                    _paramType = ty'
                  }

    goGadtType :: Concrete.RhsGadt 'Scoped -> Sem r Internal.Expression
    goGadtType = goExpression . (^. Concrete.rhsGadtType)

    goRhs :: Concrete.ConstructorRhs 'Scoped -> Sem r Internal.Expression
    goRhs = \case
      ConstructorRhsGadt r -> goGadtType r
      ConstructorRhsRecord r -> goRecordType r
      ConstructorRhsAdt r -> goAdtType r

    isRhsRecord :: Concrete.ConstructorRhs 'Scoped -> Bool
    isRhsRecord = \case
      ConstructorRhsGadt {} -> False
      ConstructorRhsRecord {} -> True
      ConstructorRhsAdt {} -> False

goLiteral :: LiteralLoc -> Internal.LiteralLoc
goLiteral = fmap go
  where
    go :: Literal -> Internal.Literal
    go = \case
      LitString s -> Internal.LitString s
      LitIntegerWithBase i -> Internal.LitNumeric (i ^. integerWithBaseValue)

goListPattern :: (Members '[Error ScoperError, NameIdGen, Reader S.InfoTable] r) => Concrete.ListPattern 'Scoped -> Sem r Internal.Pattern
goListPattern l = localBuiltins $ do
  nil_ <- getBuiltinNameScoper loc BuiltinListNil
  cons_ <- getBuiltinNameScoper loc BuiltinListCons
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

createArgumentBlocks :: NonEmpty (NamedArgumentNew 'Scoped) -> [NameBlock 'Scoped] -> [ArgumentBlock 'Scoped]
createArgumentBlocks appargs =
  run
    . execOutputList
    . evalState args0
    . mapM_ goBlock
  where
    namedArgumentRefSymbol :: NamedArgumentNew 'Scoped -> S.Symbol
    namedArgumentRefSymbol = \case
      NamedArgumentNewFunction p -> p ^. namedArgumentFunctionDef . signName
      NamedArgumentItemPun p -> over S.nameConcrete fromUnqualified' (p ^. namedArgumentReferencedSymbol . scopedIdenFinal)
    args0 :: HashSet S.Symbol = hashSet (namedArgumentRefSymbol <$> appargs)
    goBlock ::
      forall r.
      (Members '[State (HashSet S.Symbol), Output (ArgumentBlock 'Scoped)] r) =>
      NameBlock 'Scoped ->
      Sem r ()
    goBlock NameBlock {..} = do
      args <- get
      let namesInBlock :: HashSet Symbol =
            HashSet.intersection
              (HashMap.keysSet _nameBlock)
              (HashSet.map (^. S.nameConcrete) args)
          argNames :: HashMap Symbol S.Symbol = indexedByHash (^. S.nameConcrete) args
          getName sym = fromJust (argNames ^. at sym)
      whenJust (nonEmpty namesInBlock) $ \(namesInBlock1 :: NonEmpty Symbol) -> do
        let block' =
              ArgumentBlock
                { _argBlockDelims = Irrelevant Nothing,
                  _argBlockImplicit = _nameImplicit,
                  _argBlockArgs = goArg . getName <$> namesInBlock1
                }
        modify (HashSet.filter (not . flip HashSet.member namesInBlock . (^. S.nameConcrete)))
        output block'
      where
        goArg :: S.Symbol -> NamedArgumentAssign 'Scoped
        goArg sym =
          NamedArgumentAssign
            { _namedArgName = sym,
              _namedArgAssignKw = Irrelevant dummyKw,
              _namedArgValue =
                Concrete.ExpressionIdentifier
                  ScopedIden
                    { _scopedIdenFinal = name,
                      _scopedIdenAlias = Nothing
                    }
            }
          where
            name :: S.Name = over S.nameConcrete NameUnqualified sym
            dummyKw = run (runReader dummyLoc (Gen.kw Gen.kwAssign))
            dummyLoc = getLoc sym

goExpression ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  Expression ->
  Sem r Internal.Expression
goExpression = \case
  ExpressionIdentifier nt -> return (goIden nt)
  ExpressionParensIdentifier nt -> return (goIden nt)
  ExpressionApplication a -> goApplication a
  ExpressionCase a -> Internal.ExpressionCase <$> goCase a
  ExpressionIf a -> goIf a
  ExpressionInfixApplication ia -> Internal.ExpressionApplication <$> goInfix ia
  ExpressionPostfixApplication pa -> Internal.ExpressionApplication <$> goPostfix pa
  ExpressionLiteral l -> return (Internal.ExpressionLiteral (goLiteral l))
  ExpressionLambda l -> Internal.ExpressionLambda <$> goLambda l
  ExpressionBraces b -> throw (ErrAppLeftImplicit (AppLeftImplicit b))
  ExpressionDoubleBraces b -> throw (ErrDanglingDoubleBrace (DanglingDoubleBrace b))
  ExpressionLet l -> goLet l
  ExpressionDo l -> goDo l
  ExpressionList l -> goList l
  ExpressionUniverse uni -> return (Internal.ExpressionUniverse (goUniverse uni))
  ExpressionFunction func -> Internal.ExpressionFunction <$> goFunction func
  ExpressionHole h -> return (Internal.ExpressionHole h)
  ExpressionInstanceHole h -> return (Internal.ExpressionInstanceHole (fromHole h))
  ExpressionIterator i -> goIterator i
  ExpressionNamedApplicationNew i -> goNamedApplicationNew i []
  ExpressionRecordUpdate i -> goRecordUpdateApp i
  ExpressionParensRecordUpdate i -> Internal.ExpressionLambda <$> goRecordUpdate (i ^. parensRecordUpdate)
  where
    goNamedApplication :: ScopedIden -> NonEmpty (ArgumentBlock 'Scoped) -> [Internal.ApplicationArg] -> Sem r Internal.Expression
    goNamedApplication fun blocks extraArgs = do
      s <- asks (^. S.infoNameSigs)
      runReader s (runNamedArguments fun blocks extraArgs) >>= goDesugaredNamedApplication

    goNamedApplicationNew ::
      Concrete.NamedApplicationNew 'Scoped ->
      [Internal.ApplicationArg] ->
      Sem r Internal.Expression
    goNamedApplicationNew napp extraArgs = case nonEmpty (napp ^. namedApplicationNewArguments) of
      Nothing -> return (goIden (napp ^. namedApplicationNewName))
      Just appargs -> do
        let name = napp ^. namedApplicationNewName . scopedIdenFinal
        sig <- fromJust <$> asks (^. S.infoNameSigs . at (name ^. S.nameId))
        let fun = napp ^. namedApplicationNewName
            blocks = nonEmpty' (createArgumentBlocks appargs (sig ^. nameSignatureArgs))
        compiledNameApp <- goNamedApplication fun blocks extraArgs
        case nonEmpty (appargs ^.. each . _NamedArgumentNewFunction) of
          Nothing -> return compiledNameApp
          Just funs -> do
            cls <- funDefsToClauses funs
            let funsNames :: [Internal.Name] = funs ^.. each . namedArgumentFunctionDef . signName . to goSymbol
                -- changes the kind from Variable to Function
                updateKind :: Internal.Subs = Internal.subsKind funsNames KNameFunction
            let l =
                  Internal.Let
                    { _letClauses = cls,
                      _letExpression = compiledNameApp
                    }
            expr <- Internal.substitutionE updateKind l >>= Internal.inlineLet
            Internal.clone expr
            where
              funDefsToClauses :: NonEmpty (NamedArgumentFunctionDef 'Scoped) -> Sem r (NonEmpty Internal.LetClause)
              funDefsToClauses args = mkLetClauses <$> mapM goArg args
                where
                  goArg :: NamedArgumentFunctionDef 'Scoped -> Sem r Internal.PreLetStatement
                  goArg = fmap Internal.PreLetFunctionDef . goFunctionDef . (^. namedArgumentFunctionDef)

    goDesugaredNamedApplication :: DesugaredNamedApplication -> Sem r Internal.Expression
    goDesugaredNamedApplication a = do
      let fun = goScopedIden (a ^. dnamedAppIdentifier)
          updateKind :: Internal.Subs = Internal.subsKind (a ^.. dnamedAppArgs . each . argName . to goSymbol) KNameFunction
          mkAppArg :: Arg -> Internal.ApplicationArg
          mkAppArg arg =
            Internal.ApplicationArg
              { _appArgIsImplicit = arg ^. argImplicit,
                _appArg = Internal.toExpression (goSymbol (arg ^. argName))
              }
          namedArgNames :: NonEmpty Internal.ApplicationArg = mkAppArg <$> a ^. dnamedAppArgs
          allArgs = toList namedArgNames <> a ^. dnamedExtraArgs
          app = Internal.foldApplication (Internal.toExpression fun) allArgs
      clauses <- mapM mkClause (a ^. dnamedAppArgs)
      expr <-
        Internal.substitutionE updateKind $
          Internal.ExpressionLet
            Internal.Let
              { _letExpression = app,
                _letClauses = clauses
              }
      Internal.clone expr
      where
        mkClause :: Arg -> Sem r Internal.LetClause
        mkClause arg = do
          checkCycle
          let name = arg ^. argName
              adjust
                | arg ^. argAutoInserted = over defaultArgsStack (name :)
                | otherwise = id
          local adjust $ do
            body' <- goExpression (arg ^. argValue)
            ty <- goExpression (arg ^. argType)
            return $
              Internal.LetFunDef
                (Internal.simpleFunDef (goSymbol name) ty body')
          where
            checkCycle :: Sem r ()
            checkCycle = do
              st <- asks (^. defaultArgsStack)
              case span (/= (arg ^. argName)) st of
                (_, []) -> return ()
                (c, _) ->
                  let cyc = NonEmpty.reverse ((arg ^. argName) :| c)
                   in throw (ErrDefaultArgCycle (DefaultArgCycle cyc))

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

        mkArgs :: IntMap (IsImplicit, Internal.VarName) -> Sem r [Internal.ApplicationArg]
        mkArgs vs = do
          fieldMap <- mkFieldmap
          execOutputList $
            go (uncurry Indexed <$> IntMap.toAscList fieldMap) (intMapToList vs)
          where
            go :: [Indexed (RecordUpdateField 'Scoped)] -> [Indexed (IsImplicit, Internal.VarName)] -> Sem (Output Internal.ApplicationArg ': r) ()
            go fields = \case
              [] -> return ()
              Indexed idx (impl, var) : vars' -> case getArg idx of
                Nothing -> do
                  output
                    Internal.ApplicationArg
                      { _appArg = Internal.toExpression var,
                        _appArgIsImplicit = impl
                      }
                  go fields vars'
                Just (arg, fields') -> do
                  val' <- goExpression (arg ^. fieldUpdateValue)
                  output
                    Internal.ApplicationArg
                      { _appArg = val',
                        _appArgIsImplicit = impl
                      }
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
              vars :: IntMap (IsImplicit, Internal.Name) = second goSymbol <$> extra ^. recordUpdateExtraVars
              patArg = Internal.mkConstructorVarPattern Explicit constr (toList vars)
          args <- mkArgs vars
          return
            Internal.LambdaClause
              { _lambdaPatterns = pure patArg,
                _lambdaBody = Internal.foldApplication (Internal.toExpression constr) args
              }

    goRecordUpdateApp :: Concrete.RecordUpdateApp -> Sem r Internal.Expression
    goRecordUpdateApp r = do
      expr' <- goExpression (r ^. recordAppExpression)
      lam <- Internal.ExpressionLambda <$> goRecordUpdate (r ^. recordAppUpdate)
      return $ Internal.foldExplicitApplication lam [expr']

    goList :: Concrete.List 'Scoped -> Sem r Internal.Expression
    goList l = localBuiltins $ do
      nil_ <- getBuiltinNameScoper loc BuiltinListNil
      cons_ <- getBuiltinNameScoper loc BuiltinListCons
      items <- mapM goExpression (l ^. Concrete.listItems)
      return (foldr (\a b -> cons_ Internal.@@ a Internal.@@ b) (Internal.toExpression nil_) items)
      where
        loc = getLoc l

    goIf :: Concrete.If 'Scoped -> Sem r Internal.Expression
    goIf e@Concrete.If {..} = do
      if_ <- localBuiltins $ getBuiltinNameScoper (getLoc e) BuiltinBoolIf
      go if_ _ifBranches
      where
        go :: Internal.Name -> [Concrete.IfBranch 'Scoped 'BranchIfBool] -> Sem r Internal.Expression
        go if_ = \case
          [] ->
            goExpression (_ifBranchElse ^. Concrete.ifBranchExpression)
          Concrete.IfBranch {..} : brs -> do
            c <- goExpression _ifBranchCondition
            b1 <- goExpression _ifBranchExpression
            b2 <- go if_ brs
            return $ if_ Internal.@@ c Internal.@@ b1 Internal.@@ b2

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

    goApplicationArg :: Expression -> Sem r Internal.ApplicationArg
    goApplicationArg arg =
      let (e, i) = case arg of
            ExpressionBraces b -> (b ^. withLocParam, Implicit)
            ExpressionDoubleBraces b -> (b ^. doubleBracesExpression, ImplicitInstance)
            _ -> (arg, Explicit)
       in do
            e' <- goExpression e
            return (Internal.ApplicationArg i e')

    goApplication :: Application -> Sem r Internal.Expression
    goApplication a = do
      let (f, args) = unfoldApp a
      args' <- toList <$> mapM goApplicationArg args
      case f of
        ExpressionNamedApplicationNew n -> goNamedApplicationNew n args'
        _ -> do
          f' <- goExpression f
          return (Internal.foldApplication f' args')
      where
        unfoldApp :: Application -> (Expression, NonEmpty Expression)
        unfoldApp (Application l' r') =
          let (f, largs) = go [] l'
           in (f, largs |: r')
          where
            go :: [Expression] -> Expression -> (Expression, [Expression])
            go ac = \case
              ExpressionApplication (Application l r) -> go (r : ac) l
              e -> (e, ac)

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

goLetFunDefs ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  NonEmpty (LetStatement 'Scoped) ->
  Sem r [Internal.LetClause]
goLetFunDefs clauses = maybe [] (toList . mkLetClauses) . nonEmpty <$> preLetStatements clauses
  where
    preLetStatements :: NonEmpty (LetStatement 'Scoped) -> Sem r [Internal.PreLetStatement]
    preLetStatements cl = mapMaybeM preLetStatement (toList cl)
      where
        preLetStatement :: LetStatement 'Scoped -> Sem r (Maybe Internal.PreLetStatement)
        preLetStatement = \case
          LetFunctionDef f -> Just . Internal.PreLetFunctionDef <$> goFunctionDef f
          LetAliasDef {} -> return Nothing
          LetOpen {} -> return Nothing

goDo ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  Do 'Scoped ->
  Sem r Internal.Expression
goDo Do {..} = do
  goStatements _doStatements
  where
    goStatements :: NonEmpty (DoStatement 'Scoped) -> Sem r Internal.Expression
    goStatements (s :| ss) = case nonEmpty ss of
      Nothing -> goLastStatement s
      Just ss1 -> do
        ss1' <- goStatements ss1
        case s of
          DoStatementExpression e -> goDoExpression e ss1'
          DoStatementBind b -> goDoBind b ss1'
          DoStatementLet b -> goDoLet b ss1'
      where
        -- l >>= \{_ := r}
        goDoExpression :: Expression -> Internal.Expression -> Sem r Internal.Expression
        goDoExpression l r = do
          let p =
                PatternArg
                  { _patternArgIsImplicit = Explicit,
                    _patternArgName = Nothing,
                    _patternArgPattern = PatternWildcard (Wildcard (getLoc l <> getLoc r))
                  }
          goDoBindHelper p l r

        goDoLet :: DoLet 'Scoped -> Internal.Expression -> Sem r Internal.Expression
        goDoLet DoLet {..} r = do
          defs <- goLetFunDefs _doLetStatements
          return $ case nonEmpty defs of
            Nothing -> r
            Just defs1 ->
              Internal.ExpressionLet
                Internal.Let
                  { _letClauses = defs1,
                    _letExpression = r
                  }

        goDoBind :: DoBind 'Scoped -> Internal.Expression -> Sem r Internal.Expression
        goDoBind DoBind {..} r = goDoBindHelper _doBindPattern _doBindExpression r

        -- l >>= \{p := r}
        goDoBindHelper :: PatternArg -> Expression -> Internal.Expression -> Sem r Internal.Expression
        goDoBindHelper p l r = do
          p' <- goPatternArg p
          l' <- goExpression l
          bindIden <- localBuiltins (getBuiltinNameScoper (getLoc _doKeyword) BuiltinMonadBind)
          return (bindIden Internal.@@ l' Internal.@@ (p' Internal.==> r))

        goLastStatement :: DoStatement 'Scoped -> Sem r Internal.Expression
        goLastStatement = \case
          DoStatementExpression e -> goExpression e
          d -> throw (ErrDoLastStatement (DoLastStatement d))

goCase :: forall r. (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) => Case 'Scoped -> Sem r Internal.Case
goCase c = do
  _caseExpression <- goExpression (c ^. caseExpression)
  _caseBranches <- mapM goBranch (c ^. caseBranches)
  let _caseExpressionType :: Maybe Internal.Expression = Nothing
      _caseExpressionWholeType :: Maybe Internal.Expression = Nothing
  return Internal.Case {..}
  where
    goBranch :: CaseBranch 'Scoped -> Sem r Internal.CaseBranch
    goBranch b = do
      _caseBranchPattern <- goPatternArg (b ^. caseBranchPattern)
      _caseBranchRhs <- goCaseBranchRhs (b ^. caseBranchRhs)
      return Internal.CaseBranch {..}

gRhsExpression ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  RhsExpression 'Scoped ->
  Sem r Internal.Expression
gRhsExpression RhsExpression {..} = goExpression _rhsExpression

goSideIfBranch ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  SideIfBranch 'Scoped 'BranchIfBool ->
  Sem r Internal.SideIfBranch
goSideIfBranch s = do
  cond' <- goExpression (s ^. sideIfBranchCondition)
  body' <- goExpression (s ^. sideIfBranchBody)
  return
    Internal.SideIfBranch
      { _sideIfBranchCondition = cond',
        _sideIfBranchBody = body'
      }

goSideIfBranchElse ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  SideIfBranch 'Scoped 'BranchIfElse ->
  Sem r Internal.Expression
goSideIfBranchElse s = goExpression (s ^. sideIfBranchBody)

goSideIfs ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  SideIfs 'Scoped ->
  Sem r Internal.SideIfs
goSideIfs s = do
  branches' <- mapM goSideIfBranch (s ^. sideIfBranches)
  else' <- mapM goSideIfBranchElse (s ^. sideIfElse)
  return
    Internal.SideIfs
      { _sideIfBranches = branches',
        _sideIfElse = else'
      }

goCaseBranchRhs ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  CaseBranchRhs 'Scoped ->
  Sem r Internal.CaseBranchRhs
goCaseBranchRhs = \case
  CaseBranchRhsExpression e -> Internal.CaseBranchRhsExpression <$> gRhsExpression e
  CaseBranchRhsIf s -> Internal.CaseBranchRhsIf <$> goSideIfs s

goLambda :: forall r. (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) => Lambda 'Scoped -> Sem r Internal.Lambda
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

goFunction :: (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) => Function 'Scoped -> Sem r Internal.Function
goFunction f = do
  headParam :| tailParams <- goFunctionParameters (f ^. funParameters)
  ret <- goExpression (f ^. funReturn)
  return $
    Internal.Function
      { _functionLeft = headParam,
        _functionRight = foldr (\param acc -> Internal.ExpressionFunction $ Internal.Function param acc) ret tailParams
      }

goFunctionParameters ::
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
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
  (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) =>
  PatternApp ->
  Sem r Internal.ConstructorApp
goPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternApplication a)

goWildcardConstructor ::
  WildcardConstructor 'Scoped ->
  Internal.WildcardConstructor
goWildcardConstructor a = Internal.WildcardConstructor (goScopedIden (a ^. wildcardConstructor))

goPatternConstructor ::
  (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) =>
  ScopedIden ->
  Sem r Internal.ConstructorApp
goPatternConstructor a = uncurry mkConstructorApp <$> viewApp (PatternConstructor a)

goInfixPatternApplication ::
  (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) =>
  PatternInfixApp ->
  Sem r Internal.ConstructorApp
goInfixPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternInfixApplication a)

goPostfixPatternApplication ::
  (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) =>
  PatternPostfixApp ->
  Sem r Internal.ConstructorApp
goPostfixPatternApplication a = uncurry mkConstructorApp <$> viewApp (PatternPostfixApplication a)

viewApp :: forall r. (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) => Pattern -> Sem r (Internal.ConstrName, [Internal.PatternArg])
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

goPatternArg :: (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) => PatternArg -> Sem r Internal.PatternArg
goPatternArg p = do
  pat' <- goPattern (p ^. patternArgPattern)
  return
    Internal.PatternArg
      { _patternArgIsImplicit = p ^. patternArgIsImplicit,
        _patternArgName = goSymbol <$> p ^. patternArgName,
        _patternArgPattern = pat'
      }

goPattern :: (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) => Pattern -> Sem r Internal.Pattern
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

goRecordPattern :: forall r. (Members '[NameIdGen, Error ScoperError, Reader S.InfoTable] r) => RecordPattern 'Scoped -> Sem r Internal.Pattern
goRecordPattern r = do
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
    constr :: Internal.Name
    constr = goScopedIden (r ^. recordPatternConstructor)

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
      sig <-
        asks $
          fromJust
            . HashMap.lookup (constr ^. Internal.nameId)
            . (^. S.infoConstructorSigs)
      let maxIdx = length (sig ^. recordNames) - 1
      args <- IntMap.toAscList <$> byIndex
      execOutputList (go maxIdx 0 args)
      where
        loc = getLoc r
        go :: Int -> Int -> [(Int, Internal.PatternArg)] -> Sem (Output Internal.PatternArg ': r) ()
        go maxIdx idx args
          | idx > maxIdx = return ()
          | (ix', arg') : args' <- args,
            ix' == idx = do
              output arg'
              go maxIdx (idx + 1) args'
          | otherwise = do
              v <- Internal.freshVar loc ("gen_" <> show idx)
              output (Internal.patternArgFromVar Internal.Explicit v)
              go maxIdx (idx + 1) args

goAxiom :: (Members '[Reader DefaultArgsStack, Reader Pragmas, Error ScoperError, NameIdGen, Reader S.InfoTable] r) => AxiomDef 'Scoped -> Sem r Internal.AxiomDef
goAxiom a = do
  _axiomType' <- goExpression (a ^. axiomType)
  _axiomPragmas' <- goPragmas (a ^. axiomPragmas)
  let axiom =
        Internal.AxiomDef
          { _axiomType = _axiomType',
            _axiomBuiltin = (^. withLocParam) <$> a ^. axiomBuiltin,
            _axiomName = goSymbol (a ^. axiomName),
            _axiomPragmas = _axiomPragmas',
            _axiomDocComment = fmap ppPrintJudoc (a ^. axiomDoc)
          }
  whenJust (a ^. axiomBuiltin) (checkBuiltinAxiom axiom . (^. withLocParam))
  return axiom
