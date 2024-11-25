module Juvix.Compiler.Internal.Translation.FromConcrete
  ( module Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context,
    fromConcrete,
    DefaultArgsStack,
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
import Juvix.Compiler.Internal.Data.InfoTable qualified as Internal
import Juvix.Compiler.Internal.Data.NameDependencyInfo qualified as Internal
import Juvix.Compiler.Internal.Extra (mkLetClauses)
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Extra.DependencyBuilder
import Juvix.Compiler.Internal.Language (varFromWildcard)
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra qualified as Store
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Compiler.Store.Scoped.Data.InfoTable qualified as S
import Juvix.Compiler.Store.Scoped.Language (createExportsTable)
import Juvix.Compiler.Store.Scoped.Language qualified as S
import Juvix.Prelude
import Safe (lastMay)

-- | Needed to generate field projections and deriving instances
data LocalTable = LocalTable
  { _localInfoConstructors :: HashMap Internal.ConstructorName ConstructorInfo,
    _localInfoInductives :: HashMap Internal.InductiveName InductiveInfo
  }

emptyLocalTable :: LocalTable
emptyLocalTable =
  LocalTable
    { _localInfoConstructors = mempty,
      _localInfoInductives = mempty
    }

makeLenses ''LocalTable

-- | Needed to detect looping while inserting default arguments
newtype DefaultArgsStack = DefaultArgsStack
  { _defaultArgsStack :: [S.Symbol]
  }
  deriving newtype (Semigroup, Monoid)

makeLenses ''DefaultArgsStack

data DerivingArgs = DerivingArgs
  { _derivingPragmas :: Maybe ParsedPragmas,
    _derivingInstanceName :: Internal.FunctionName,
    _derivingParameters :: [Internal.FunctionParameter],
    _derivingReturnType :: (Internal.InductiveName, [Internal.ApplicationArg])
  }

fromConcrete ::
  (Members '[Reader EntryPoint, Error JuvixError, Reader Store.ModuleTable, NameIdGen, Termination] r) =>
  Scoper.ScoperResult ->
  Sem r InternalResult
fromConcrete _resultScoper = do
  mtab <- ask
  let it :: InternalModuleTable = Store.getInternalModuleTable mtab
      ms :: [Store.ModuleInfo] = HashMap.elems (mtab ^. Store.moduleTable)
      exportTbl =
        _resultScoper ^. Scoper.resultExports
          <> mconcatMap (createExportsTable . (^. Store.moduleInfoScopedModule . S.scopedModuleExportInfo)) ms
      internalTable :: Internal.InfoTable = computeCombinedInfoTable it
      tab :: S.InfoTable =
        S.getCombinedInfoTable (_resultScoper ^. Scoper.resultScopedModule)
          <> mconcatMap (S.getCombinedInfoTable . (^. Store.moduleInfoScopedModule)) ms
  _resultModule <-
    runReader @Pragmas mempty
      . runReader @ExportsTable exportTbl
      . runReader tab
      . runReader internalTable
      . mapError (JuvixError @ScoperError)
      . evalState emptyLocalTable
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
  (Members '[Reader EntryPoint, State LocalTable, Reader DefaultArgsStack, Error ScoperError, NameIdGen, Reader Pragmas, Reader S.InfoTable] r) =>
  Module 'Scoped 'ModuleLocal ->
  Sem r [Internal.PreStatement]
goLocalModule = concatMapM goAxiomInductive . (^. moduleBody)

goTopModule ::
  (Members '[Reader DefaultArgsStack, Reader EntryPoint, Reader ExportsTable, Error JuvixError, Error ScoperError, NameIdGen, Reader Pragmas, Termination, Reader S.InfoTable, Reader Internal.InfoTable] r) =>
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
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, Reader ExportsTable, Error ScoperError, NameIdGen, Reader Pragmas, Reader S.InfoTable, Reader Internal.InfoTable] r) =>
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
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, Reader ExportsTable, Error ScoperError, NameIdGen, Reader Pragmas, Reader S.InfoTable, Reader Internal.InfoTable] r) =>
  [Statement 'Scoped] ->
  Sem r Internal.PreModuleBody
goModuleBody stmts = evalState emptyLocalTable $ do
  _moduleImports <- mapM goImport (scanImports stmts)
  otherThanFunctions :: [Indexed Internal.PreStatement] <- concatMapM (traverseM' goAxiomInductive) ss
  funs :: [Indexed Internal.PreStatement] <-
    concat
      <$> sequence
        [ return . map (Indexed i . Internal.PreFunctionDef) =<< defs
          | Indexed i s <- ss,
            let defs = mkFunctionLike s
        ]
  let unsorted = otherThanFunctions <> funs
      _moduleStatements = map (^. indexedThing) (sortOn (^. indexedIx) unsorted)
  return Internal.ModuleBody {..}
  where
    ss' :: [Statement 'Scoped]
    ss' = concatMap Concrete.flattenStatement stmts

    ss :: [Indexed (Statement 'Scoped)]
    ss = zipWith Indexed [0 ..] ss'

    mkFunctionLike :: Statement 'Scoped -> Sem (State LocalTable ': r) [Internal.FunctionDef]
    mkFunctionLike s = case s of
      StatementFunctionDef d -> goFunctionDef d
      StatementProjectionDef d -> goProjectionDef d >>= return . pure
      StatementDeriving d -> goDeriving d >>= return . pure
      StatementSyntax {} -> return []
      StatementImport {} -> return []
      StatementInductive {} -> return []
      StatementModule {} -> return []
      StatementOpenModule {} -> return []
      StatementAxiom {} -> return []

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
      StatementDeriving {} -> []
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
  (Members '[Reader EntryPoint, Reader DefaultArgsStack, State LocalTable, Error ScoperError, NameIdGen, Reader Pragmas, Reader S.InfoTable] r) =>
  Statement 'Scoped ->
  Sem r [Internal.PreStatement]
goAxiomInductive = \case
  StatementInductive i -> pure . Internal.PreInductiveDef <$> goInductive i
  StatementAxiom d -> pure . Internal.PreAxiomDef <$> goAxiom d
  StatementModule f -> goLocalModule f
  StatementImport {} -> return []
  StatementFunctionDef {} -> return []
  StatementDeriving {} -> return []
  StatementSyntax {} -> return []
  StatementOpenModule {} -> return []
  StatementProjectionDef {} -> return []

goProjectionDef ::
  forall r.
  (Members '[Reader DefaultArgsStack, State LocalTable, Reader Pragmas, NameIdGen, Error ScoperError, Reader S.InfoTable] r) =>
  ProjectionDef 'Scoped ->
  Sem r Internal.FunctionDef
goProjectionDef ProjectionDef {..} = do
  let c = goSymbol _projectionConstructor
  info <- gets (^?! localInfoConstructors . at c . _Just)
  let field = goSymbol _projectionField
  msig <- asks (^. S.infoNameSigs . at (field ^. Internal.nameId))
  argInfos <- maybe (return mempty) (fmap toList . goNameSignature) msig
  projType <- goExpression _projectionType
  fun <-
    Internal.genFieldProjection
      _projectionKind
      field
      projType
      ( (^. withLocParam)
          <$> _projectionFieldBuiltin
      )
      argInfos
      (fmap (^. withLocParam . withSourceValue) _projectionPragmas)
      info
      _projectionFieldIx
  whenJust (fun ^. Internal.funDefBuiltin) (checkBuiltinFunction fun)
  return fun

goNameSignature ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  NameSignature 'Scoped ->
  Sem r [Internal.ArgInfo]
goNameSignature = mconcatMapM (fmap toList . goBlock) . (^. nameSignatureArgs)
  where
    goBlock :: NameBlock 'Scoped -> Sem r (NonEmpty Internal.ArgInfo)
    goBlock blk = mapM goItem (blk ^. nameBlockItems)
      where
        goItem :: NameItem 'Scoped -> Sem r Internal.ArgInfo
        goItem i = do
          _argInfoDefault' <- mapM goExpression (i ^? nameItemDefault . _Just . argDefaultValue)
          return
            Internal.ArgInfo
              { _argInfoDefault = _argInfoDefault',
                _argInfoName = goSymbol <$> (i ^. nameItemSymbol)
              }

goDeriving ::
  forall r.
  (Members '[Reader DefaultArgsStack, Reader Pragmas, Error ScoperError, NameIdGen, State LocalTable, Reader Internal.InfoTable, Reader S.InfoTable] r) =>
  Deriving 'Scoped ->
  Sem r Internal.FunctionDef
goDeriving Deriving {..} = do
  let FunctionLhs {..} = _derivingFunLhs
      name = goSymbol (_funLhsName ^. functionDefName)
  (funArgs, ret) <- Internal.unfoldFunType <$> goDefType _derivingFunLhs
  let (mtrait, traitArgs) = Internal.unfoldExpressionApp ret
  (n, der) <- findDerivingTrait mtrait
  let deriveArgs =
        DerivingArgs
          { _derivingInstanceName = name,
            _derivingReturnType = (n, traitArgs),
            _derivingParameters = funArgs,
            _derivingPragmas
          }
  deriveTrait der deriveArgs

deriveTrait ::
  ( Members
      '[ Reader S.InfoTable,
         Reader Pragmas,
         Reader DefaultArgsStack,
         Error ScoperError,
         Reader Internal.InfoTable,
         State LocalTable,
         NameIdGen
       ]
      r
  ) =>
  Internal.DerivingTrait ->
  DerivingArgs ->
  Sem r Internal.FunctionDef
deriveTrait = \case
  Internal.DerivingEq -> deriveEq

findDerivingTrait ::
  forall r.
  ( Members
      '[ Error ScoperError,
         Reader S.InfoTable
       ]
      r
  ) =>
  Internal.Expression ->
  Sem r (Internal.Name, Internal.DerivingTrait)
findDerivingTrait ret = do
  i :: Internal.Name <- maybe err return (ret ^? Internal._ExpressionIden . Internal._IdenInductive)
  tbl :: BuiltinsTable <- asks (^. S.infoBuiltins)
  let matches :: Internal.DerivingTrait -> Bool
      matches t = Just i == (goSymbol <$> tbl ^. at (toBuiltinPrim t))
  (i,) <$> maybe err return (find matches allElements)
  where
    err :: Sem r a
    err = throwDerivingWrongForm ret

goArgsInfo ::
  ( Members
      '[ Reader S.InfoTable,
         NameIdGen,
         Error ScoperError,
         Reader DefaultArgsStack,
         Reader Pragmas
       ]
      r
  ) =>
  Internal.Name ->
  Sem r [Internal.ArgInfo]
goArgsInfo name = do
  msig <- asks (^. S.infoNameSigs . at (name ^. Internal.nameId))
  maybe (return mempty) (fmap toList . goNameSignature) msig

getBuiltin ::
  (IsBuiltin builtin, Members '[Reader S.InfoTable, Error ScoperError] r) =>
  Interval ->
  builtin ->
  Sem r Internal.Name
getBuiltin loc b = do
  r <- fmap goSymbol <$> asks (^. S.infoBuiltins . at (toBuiltinPrim b))
  maybe (throw err) return r
  where
    err :: ScoperError
    err =
      ErrBuiltinNotDefined
        BuiltinNotDefined
          { _notDefinedLoc = loc,
            _notDefinedBuiltin = toBuiltinPrim b
          }

getDefinedConstructor ::
  (Members '[Reader Internal.InfoTable, State LocalTable] r) =>
  Internal.ConstructorName ->
  Sem r ConstructorInfo
getDefinedConstructor ind = do
  tbl1 <- gets (^. localInfoConstructors . at ind)
  tbl2 <- asks (^. infoConstructors . at ind)
  return (fromJust (tbl1 <|> tbl2))

getDefinedInductive ::
  (Members '[Reader Internal.InfoTable, State LocalTable] r) =>
  Internal.InductiveName ->
  Sem r InductiveInfo
getDefinedInductive ind = do
  tbl1 <- gets (^. localInfoInductives . at ind)
  tbl2 <- asks (^. infoInductives . at ind)
  return (fromJust (tbl1 <|> tbl2))

throwDerivingWrongForm :: (Members '[Error ScoperError, Reader S.InfoTable] r) => Internal.Expression -> Sem r a
throwDerivingWrongForm ret = do
  let getSym :: (BuiltinPrim, S.Symbol) -> Maybe Internal.Name
      getSym (p, s) = do
        guard (isJust (Internal.derivingTraitFromBuiltin p))
        return (goSymbol s)
  _derivingTypeSupportedBuiltins <-
    mapMaybe getSym . HashMap.toList
      <$> asks (^. S.infoBuiltins)
  throw $
    ErrDerivingTypeWrongForm
      DerivingTypeWrongForm
        { _derivingTypeWrongForm = ret,
          _derivingTypeBuiltin = Internal.DerivingEq,
          _derivingTypeSupportedBuiltins
        }

deriveEq ::
  forall r.
  ( Members
      '[ Reader S.InfoTable,
         Reader Internal.InfoTable,
         State LocalTable,
         NameIdGen,
         Error ScoperError,
         Reader DefaultArgsStack,
         Reader Pragmas
       ]
      r
  ) =>
  DerivingArgs ->
  Sem r Internal.FunctionDef
deriveEq DerivingArgs {..} = do
  arg <- getArg
  argty <- getArgType
  argsInfo <- goArgsInfo _derivingInstanceName
  lamName <- Internal.freshFunVar (getLoc _derivingInstanceName) ("__eq__" <> _derivingInstanceName ^. Internal.nameText)
  let lam = Internal.ExpressionIden (Internal.IdenFunction lamName)
  lamFun <- eqLambda lam arg argty
  lamTy <- Internal.ExpressionHole <$> Internal.freshHole (getLoc _derivingInstanceName)
  let lamDef =
        Internal.FunctionDef
          { _funDefTerminating = False,
            _funDefIsInstanceCoercion = Nothing,
            _funDefPragmas = mempty,
            _funDefArgsInfo = [],
            _funDefDocComment = Nothing,
            _funDefName = lamName,
            _funDefType = lamTy,
            _funDefBody = lamFun,
            _funDefBuiltin = Nothing
          }
  mkEq <- getBuiltin (getLoc eqName) BuiltinMkEq
  pragmas' <- goPragmas _derivingPragmas
  let body =
        Internal.ExpressionLet
          Internal.Let
            { _letClauses = pure (Internal.LetMutualBlock (Internal.MutualBlockLet (pure lamDef))),
              _letExpression = mkEq Internal.@@ lam
            }
      ty = Internal.foldFunType _derivingParameters ret
  return
    Internal.FunctionDef
      { _funDefTerminating = False,
        _funDefIsInstanceCoercion = Just Internal.IsInstanceCoercionInstance,
        _funDefPragmas = pragmas',
        _funDefArgsInfo = argsInfo,
        _funDefDocComment = Nothing,
        _funDefName = _derivingInstanceName,
        _funDefType = ty,
        _funDefBody = body,
        _funDefBuiltin = Nothing
      }
  where
    ret :: Internal.Expression
    ret = Internal.foldApplication (Internal.toExpression eqName) args

    eqName :: Internal.InductiveName
    args :: [Internal.ApplicationArg]
    (eqName, args) = _derivingReturnType

    getArg :: Sem r Internal.InductiveInfo
    getArg = runFailDefaultM (throwDerivingWrongForm ret) $ do
      [Internal.ApplicationArg Explicit a] <- return args
      Internal.ExpressionIden (Internal.IdenInductive ind) <- return (fst (Internal.unfoldExpressionApp a))
      getDefinedInductive ind

    getArgType :: Sem r Internal.Expression
    getArgType = runFailDefaultM (throwDerivingWrongForm ret) $ do
      [Internal.ApplicationArg Explicit a] <- return args
      return a

    eqLambda :: Internal.Expression -> Internal.InductiveInfo -> Internal.Expression -> Sem r Internal.Expression
    eqLambda lam d argty = do
      let loc = getLoc eqName
      band <- getBuiltin loc BuiltinBoolAnd
      btrue <- getBuiltin loc BuiltinBoolTrue
      bfalse <- getBuiltin loc BuiltinBoolFalse
      bisEqual <- getBuiltin loc BuiltinIsEqual
      case nonEmpty (d ^. Internal.inductiveInfoConstructors) of
        Nothing -> return (Internal.toExpression btrue)
        Just cs -> do
          cl' <- mapM (lambdaClause band btrue bisEqual) cs
          defaultCl' <-
            if
                | notNull (NonEmpty.tail cs) -> Just <$> defaultLambdaClause bfalse
                | otherwise -> return Nothing
          return
            ( Internal.ExpressionLambda
                Internal.Lambda
                  { _lambdaType = Nothing,
                    _lambdaClauses = snocNonEmptyMaybe cl' defaultCl'
                  }
            )
      where
        defaultLambdaClause :: Internal.Name -> Sem r Internal.LambdaClause
        defaultLambdaClause btrue = do
          let loc = getLoc eqName
          p1 <- Internal.genWildcard loc Internal.Explicit
          p2 <- Internal.genWildcard loc Internal.Explicit
          return
            Internal.LambdaClause
              { _lambdaPatterns = p1 :| [p2],
                _lambdaBody = Internal.toExpression btrue
              }

        lambdaClause ::
          Internal.FunctionName ->
          Internal.FunctionName ->
          Internal.FunctionName ->
          Internal.ConstructorName ->
          Sem r Internal.LambdaClause
        lambdaClause band btrue bisEqual c = do
          argsRecursive :: [Bool] <- getRecursiveArgs
          numArgs :: [IsImplicit] <- getNumArgs
          let loc = getLoc _derivingInstanceName
              mkpat :: Sem r ([Internal.VarName], Internal.PatternArg)
              mkpat = runOutputList . runStreamOf allWords $ do
                xs :: [(IsImplicit, Internal.VarName)] <- forM numArgs $ \impl -> do
                  v <- yield >>= Internal.freshVar loc
                  output v
                  return (impl, v)
                return (Internal.mkConstructorVarPattern Explicit c xs)
          (v1, p1) <- mkpat
          (v2, p2) <- mkpat
          return
            Internal.LambdaClause
              { _lambdaPatterns = p1 :| [p2],
                _lambdaBody = allEq (zip3Exact v1 v2 argsRecursive)
              }
          where
            allEq :: (Internal.IsExpression expr) => [(expr, expr, Bool)] -> Internal.Expression
            allEq k = case nonEmpty k of
              Nothing -> Internal.toExpression btrue
              Just l -> mkAnds (fmap (uncurry3 mkEq) l)

            mkAnds :: (Internal.IsExpression expr) => NonEmpty expr -> Internal.Expression
            mkAnds = foldl1 mkAnd . fmap Internal.toExpression

            mkAnd :: (Internal.IsExpression expr) => expr -> expr -> Internal.Expression
            mkAnd a b = band Internal.@@ a Internal.@@ b

            mkEq :: (Internal.IsExpression expr) => expr -> expr -> Bool -> Internal.Expression
            mkEq a b isRec
              | isRec = lam Internal.@@ a Internal.@@ b
              | otherwise = bisEqual Internal.@@ a Internal.@@ b

            getNumArgs :: Sem r [IsImplicit]
            getNumArgs = do
              def <- getDefinedConstructor c
              return $
                def
                  ^.. Internal.constructorInfoType
                    . to Internal.constructorArgs
                    . each
                    . Internal.paramImplicit

            getRecursiveArgs :: Sem r [Bool]
            getRecursiveArgs = do
              def <- getDefinedConstructor c
              let argTypes = map (^. Internal.paramType) $ Internal.constructorArgs (def ^. Internal.constructorInfoType)
              return $ map (== argty) argTypes

goFunctionDef ::
  forall r.
  (Members '[Reader DefaultArgsStack, Reader Pragmas, Error ScoperError, NameIdGen, Reader S.InfoTable] r) =>
  FunctionDef 'Scoped ->
  Sem r [Internal.FunctionDef]
goFunctionDef def@FunctionDef {..} = do
  let _funDefName = goSymbol (_signName ^. functionDefName)
      _funDefTerminating = isJust _signTerminating
      _funDefIsInstanceCoercion
        | isJust _signCoercion = Just Internal.IsInstanceCoercionCoercion
        | isJust _signInstance = Just Internal.IsInstanceCoercionInstance
        | otherwise = Nothing
      _funDefCoercion = isJust _signCoercion
      _funDefBuiltin = (^. withLocParam) <$> _signBuiltin
  _funDefType <- goDefType (functionDefLhs def)
  _funDefPragmas <- goPragmas _signPragmas
  _funDefBody <- goBody
  _funDefArgsInfo <- goArgsInfo _funDefName
  let _funDefDocComment = fmap ppPrintJudoc _signDoc
      fun = Internal.FunctionDef {..}
  whenJust _signBuiltin (checkBuiltinFunction fun . (^. withLocParam))
  case _signName ^. functionDefNamePattern of
    Just pat -> do
      pat' <- goPatternArg pat
      (fun :) <$> Internal.genPatternDefs _funDefName pat'
    Nothing ->
      return [fun]
  where
    goBody :: Sem r Internal.Expression
    goBody = do
      commonPatterns <- concatMapM (fmap toList . argToPattern) (_signTypeSig ^. typeSigArgs)
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

argToPattern ::
  forall r.
  (Members '[NameIdGen] r) =>
  SigArg 'Scoped ->
  Sem r (NonEmpty Internal.PatternArg)
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

goDefType ::
  forall r.
  (Members '[Reader DefaultArgsStack, NameIdGen, Error ScoperError, Reader Pragmas, Reader S.InfoTable] r) =>
  FunctionLhs 'Scoped ->
  Sem r Internal.Expression
goDefType FunctionLhs {..} = do
  args <- concatMapM (fmap toList . argToParam) (_funLhsTypeSig ^. typeSigArgs)
  ret <- maybe freshHole goExpression (_funLhsTypeSig ^. typeSigRetType)
  return (Internal.foldFunType args ret)
  where
    freshHole :: Sem r Internal.Expression
    freshHole =
      Internal.ExpressionHole
        <$> Internal.freshHole (maybe (getLoc _funLhsName) getLoc (lastMay (_funLhsTypeSig ^. typeSigArgs)))

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
  BuiltinEq -> checkEqDef d
  BuiltinBool -> checkBoolDef d
  BuiltinInt -> checkIntDef d
  BuiltinList -> checkListDef d
  BuiltinMaybe -> checkMaybeDef d
  BuiltinPair -> checkPairDef d
  BuiltinPoseidonState -> checkPoseidonStateDef d
  BuiltinEcPoint -> checkEcPointDef d
  BuiltinAnomaResource -> checkResource d
  BuiltinAnomaAction -> checkAction d

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
  BuiltinIsEqual -> checkIsEq d
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
  BuiltinAnomaSha256 -> checkAnomaSha256 d
  BuiltinAnomaDelta -> checkDelta d
  BuiltinAnomaKind -> checkKind d
  BuiltinAnomaResourceCommitment -> checkResourceCommitment d
  BuiltinAnomaResourceNullifier -> checkResourceNullifier d
  BuiltinAnomaResourceKind -> checkResourceKind d
  BuiltinAnomaResourceDelta -> checkResourceDelta d
  BuiltinAnomaActionDelta -> checkActionDelta d
  BuiltinAnomaActionsDelta -> checkActionsDelta d
  BuiltinAnomaZeroDelta -> checkZeroDelta d
  BuiltinAnomaAddDelta -> checkDeltaBinaryOp d
  BuiltinAnomaSubDelta -> checkDeltaBinaryOp d
  BuiltinAnomaProveDelta -> checkProveDelta d
  BuiltinAnomaProveAction -> checkProveAction d
  BuiltinAnomaRandomGenerator -> checkAnomaRandomGenerator d
  BuiltinAnomaRandomGeneratorInit -> checkAnomaRandomGeneratorInit d
  BuiltinAnomaRandomNextBytes -> checkAnomaRandomNextBytes d
  BuiltinAnomaRandomSplit -> checkAnomaRandomSplit d
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
  ( Members
      '[ Reader EntryPoint,
         Reader DefaultArgsStack,
         State LocalTable,
         NameIdGen,
         Reader Pragmas,
         Error ScoperError,
         Reader S.InfoTable
       ]
      r
  ) =>
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

-- | Stores constructors so we can access them for generating field projections and deriving instances
checkInductiveConstructors :: (Members '[State LocalTable] r) => Internal.InductiveDef -> Sem r ()
checkInductiveConstructors indDef = do
  let tinfo = inductiveInfoFromInductiveDef indDef
  modify (set (localInfoInductives . at (indDef ^. Internal.inductiveName)) (Just tinfo))
  forM_ (mkConstructorEntries indDef) $ \(cname, cinfo) ->
    modify (over localInfoConstructors (HashMap.insert cname cinfo))

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
            ty' <- goTypeSig _fieldTypeSig
            return $
              Just
                Internal.FunctionParameter
                  { _paramName = Just (goSymbol _fieldName),
                    _paramImplicit = fromIsImplicitField _fieldIsImplicit,
                    _paramType = ty'
                  }

    goGadtType :: Concrete.RhsGadt 'Scoped -> Sem r Internal.Expression
    goGadtType = goTypeSig . (^. Concrete.rhsGadtTypeSig)

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

createArgumentBlocks :: NonEmpty (NamedArgumentNew 'Scoped) -> [NameBlock 'Scoped] -> NonEmpty (ArgumentBlock 'Scoped)
createArgumentBlocks appargs =
  nonEmpty'
    . run
    . execOutputList
    . evalState args0
    . mapM_ goBlock
  where
    namedArgumentRefSymbol :: NamedArgumentNew 'Scoped -> S.Symbol
    namedArgumentRefSymbol = \case
      NamedArgumentNewFunction p -> p ^. namedArgumentFunctionDef . signName . functionDefName
      NamedArgumentItemPun p -> over S.nameConcrete fromUnqualified' (p ^. namedArgumentReferencedSymbol . scopedIdenFinal)
    args0 :: HashSet S.Symbol = hashSet (namedArgumentRefSymbol <$> appargs)
    goBlock ::
      forall r.
      (Members '[State (HashSet S.Symbol), Output (ArgumentBlock 'Scoped)] r) =>
      NameBlock 'Scoped ->
      Sem r ()
    goBlock b@NameBlock {..} = do
      args <- get
      let namesInBlock :: HashSet Symbol =
            HashSet.intersection
              (hashSet (symbolParsed <$> b ^.. nameBlockSymbols))
              (HashSet.map (^. S.nameConcrete) args)
          argNames :: HashMap Symbol S.Symbol = indexedByHash (^. S.nameConcrete) args
          getName sym = fromJust (argNames ^. at sym)
      whenJust (nonEmpty namesInBlock) $ \(namesInBlock1 :: NonEmpty Symbol) -> do
        let block' =
              ArgumentBlock
                { _argBlockDelims = Irrelevant Nothing,
                  _argBlockImplicit = _nameBlockImplicit,
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
    goNamedApplication' :: ScopedIden -> NonEmpty (ArgumentBlock 'Scoped) -> [Internal.ApplicationArg] -> Sem r Internal.Expression
    goNamedApplication' fun blocks extraArgs = do
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
            blocks = createArgumentBlocks appargs (sig ^. nameSignatureArgs)
        compiledNameApp <- goNamedApplication' fun blocks extraArgs
        case nonEmpty (appargs ^.. each . _NamedArgumentNewFunction) of
          Nothing -> return compiledNameApp
          Just funs -> do
            cls <- funDefsToClauses funs
            let funsNames :: [Internal.Name] =
                  funs
                    ^.. each
                      . namedArgumentFunctionDef
                      . signName
                      . functionDefName
                      . to goSymbol
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
              funDefsToClauses args = (mkLetClauses . nonEmpty') <$> concatMapM goArg (toList args)
                where
                  goArg :: NamedArgumentFunctionDef 'Scoped -> Sem r [Internal.PreLetStatement]
                  goArg = fmap (map Internal.PreLetFunctionDef) . goFunctionDef . (^. namedArgumentFunctionDef)

    goDesugaredNamedApplication :: DesugaredNamedApplication -> Sem r Internal.Expression
    goDesugaredNamedApplication a = do
      let fun = goScopedIden (a ^. dnamedAppIdentifier)
          updateKind :: Internal.Subs =
            Internal.subsKind
              ( a
                  ^.. dnamedAppArgs
                    . each
                    . argName
                    . _Just
                    . to goSymbol
              )
              KNameFunction
          mkAppArg :: Arg -> Sem r Internal.ApplicationArg
          mkAppArg arg = do
            expr <- case arg ^. argName of
              Nothing -> goExpression (arg ^. argValue)
              Just argname -> return (Internal.toExpression (goSymbol argname))

            return
              Internal.ApplicationArg
                { _appArgIsImplicit = arg ^. argImplicit,
                  _appArg = expr
                }
      namedArgNames :: NonEmpty Internal.ApplicationArg <- mapM mkAppArg (a ^. dnamedAppArgs)
      let allArgs = toList namedArgNames <> a ^. dnamedExtraArgs
          app = Internal.foldApplication (Internal.toExpression fun) allArgs
      clauses :: [Internal.LetClause] <- mapMaybeM mkClause (toList (a ^. dnamedAppArgs))

      expr <-
        Internal.substitutionE updateKind $
          case nonEmpty clauses of
            Nothing -> app
            Just clauses1 ->
              Internal.ExpressionLet
                Internal.Let
                  { _letExpression = app,
                    _letClauses = clauses1
                  }
      Internal.clone expr
      where
        mkClause :: Arg -> Sem r (Maybe Internal.LetClause)
        mkClause arg = case arg ^. argName of
          Nothing -> return Nothing
          Just name -> do
            checkCycle
            let adjust :: DefaultArgsStack -> DefaultArgsStack
                  | arg ^. argAutoInserted = over defaultArgsStack (name :)
                  | otherwise = id
            local adjust $ do
              body' <- goExpression (arg ^. argValue)
              ty <- goExpression (arg ^. argType)
              return $
                Just
                  ( Internal.LetFunDef
                      (Internal.simpleFunDef (goSymbol name) ty body')
                  )
            where
              checkCycle :: Sem r ()
              checkCycle = do
                st <- asks (^. defaultArgsStack)
                case span (/= name) st of
                  (_, []) -> return ()
                  (c, _) ->
                    let cyc = NonEmpty.reverse (name :| c)
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
    preLetStatements cl = concatMapM preLetStatement (toList cl)
      where
        preLetStatement :: LetStatement 'Scoped -> Sem r [Internal.PreLetStatement]
        preLetStatement = \case
          LetFunctionDef f -> map Internal.PreLetFunctionDef <$> goFunctionDef f
          LetAliasDef {} -> return []
          LetOpen {} -> return []

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

goTypeSig :: (Members '[Reader DefaultArgsStack, Reader Pragmas, Error ScoperError, NameIdGen, Reader S.InfoTable] r) => TypeSig 'Scoped -> Sem r Internal.Expression
goTypeSig s = do
  ty <- Gen.mkTypeSigType s
  goExpression ty

goAxiom :: (Members '[Reader DefaultArgsStack, Reader Pragmas, Error ScoperError, NameIdGen, Reader S.InfoTable] r) => AxiomDef 'Scoped -> Sem r Internal.AxiomDef
goAxiom a = do
  _axiomType' <- goTypeSig (a ^. axiomTypeSig)
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
