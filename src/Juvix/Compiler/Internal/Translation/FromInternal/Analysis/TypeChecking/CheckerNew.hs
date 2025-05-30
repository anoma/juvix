module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error,
    checkTopModule,
    withEmptyInsertedArgsStack,
    inferExpressionRepl,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Data.Cast
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Data.TypedInstanceHole
import Juvix.Compiler.Internal.Extra hiding (freshHole)
import Juvix.Compiler.Internal.Extra qualified as Extra
import Juvix.Compiler.Internal.Extra.CoercionInfo
import Juvix.Compiler.Internal.Extra.InstanceInfo
import Juvix.Compiler.Internal.Pretty as Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker (Termination)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew.Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.ResultBuilder
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Resolver
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Traits.Termination
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

data FunctionDefaultInfo = FunctionDefaultInfo
  { _functionDefaultArgId :: ArgId,
    _functionDefaultValue :: Expression
  }
  deriving stock (Generic, Data)

data FunctionDefault = FunctionDefault
  { _functionDefaultLeft :: FunctionParameter,
    _functionDefaultDefault :: Maybe FunctionDefaultInfo,
    _functionDefaultRight :: BuilderType
  }
  deriving stock (Generic, Data)

data BuilderType
  = BuilderTypeNoDefaults Expression
  | BuilderTypeDefaults FunctionDefault
  deriving stock (Generic, Data)

data IsDefault
  = ItIsDefault ArgId
  | ItIsNotDefault
  deriving stock (Generic, Data)

data AppBuilderArg = AppBuilderArg
  { _appBuilderArgIsDefault :: IsDefault,
    _appBuilderArg :: ApplicationArg
  }
  deriving stock (Generic, Data)

data AppBuilder = AppBuilder
  { _appBuilderLeft :: Expression,
    _appBuilderType :: BuilderType,
    _appBuilderTypeCtx :: Subs,
    _appBuilderArgs :: [AppBuilderArg]
  }

newtype TypeHint = TypeHint
  { _typeHint :: Maybe Expression
  }

makeLenses ''TypeHint
makeLenses ''AppBuilder
makeLenses ''AppBuilderArg
makeLenses ''FunctionDefault
makeLenses ''FunctionDefaultInfo

mkTypeHint :: Maybe Expression -> TypeHint
mkTypeHint ty =
  TypeHint
    { _typeHint = ty
    }

emptyTypeHint :: TypeHint
emptyTypeHint =
  TypeHint
    { _typeHint = Nothing
    }

instance PrettyCode FunctionDefault where
  ppCode _ = return "ppCode(FunctionDefault)"

instance PrettyCode AppBuilderArg where
  ppCode a = Pretty.ppCode (a ^. appBuilderArg)

instance PrettyCode BuilderType where
  ppCode _ = return "ppCode(BuilderType)"

instance HasExpressions FunctionDefaultInfo where
  directExpressions f i = do
    val' <- directExpressions f (i ^. functionDefaultValue)
    pure
      FunctionDefaultInfo
        { _functionDefaultValue = val',
          _functionDefaultArgId = i ^. functionDefaultArgId
        }

instance HasExpressions FunctionDefault where
  directExpressions f FunctionDefault {..} = do
    l' <- directExpressions f _functionDefaultLeft
    r' <- directExpressions f _functionDefaultRight
    d' <- directExpressions f _functionDefaultDefault
    pure
      FunctionDefault
        { _functionDefaultLeft = l',
          _functionDefaultRight = r',
          _functionDefaultDefault = d'
        }

instance HasExpressions BuilderType where
  directExpressions f = \case
    BuilderTypeNoDefaults e -> BuilderTypeNoDefaults <$> directExpressions f e
    BuilderTypeDefaults l -> BuilderTypeDefaults <$> directExpressions f l

instance HasExpressions AppBuilderArg where
  directExpressions f AppBuilderArg {..} = do
    a' <- directExpressions f _appBuilderArg
    pure
      AppBuilderArg
        { _appBuilderArg = a',
          _appBuilderArgIsDefault
        }

registerConstructor :: (Members '[HighlightBuilder, Reader InfoTable, ResultBuilder] r) => ConstructorDef -> Sem r ()
registerConstructor ctr = do
  ty <- lookupConstructorType (ctr ^. inductiveConstructorName)
  registerNameIdType (ctr ^. inductiveConstructorName . nameId) ty

registerNameIdType :: (Members '[HighlightBuilder, ResultBuilder] r) => NameId -> Expression -> Sem r ()
registerNameIdType uid ty = do
  addIdenType uid ty
  highlightType uid ty

checkCoercionCycles ::
  (Members '[ResultBuilder, Error TypeCheckerError] r) =>
  Sem r ()
checkCoercionCycles = do
  ctab <- (^. typeCheckingTablesCoercionTable) <$> getCombinedTables
  let cyclic = cyclicCoercions ctab
      s = toList cyclic
  when (any (notDecreasing cyclic ctab) s) $
    throw (ErrCoercionCycles (CoercionCycles (nonEmpty' s)))
  where
    notDecreasing :: HashSet Name -> CoercionTable -> Name -> Bool
    notDecreasing cyclic ctab n =
      any
        ( \ci ->
            not (ci ^. coercionInfoDecreasing)
              && HashSet.member (ci ^. coercionInfoTarget . instanceAppHead . instanceAppHeadName) cyclic
        )
        cis
      where
        cis = fromJust $ HashMap.lookup n (ctab ^. coercionTableMap)

checkTopModule ::
  (Members '[HighlightBuilder, Reader BuiltinsTable, Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, ResultBuilder, Termination] r) =>
  Module ->
  Sem r Module
checkTopModule md = do
  checkCoercionCycles
  checkModule md

checkModule ::
  (Members '[HighlightBuilder, Reader BuiltinsTable, Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, ResultBuilder, Termination] r) =>
  Module ->
  Sem r Module
checkModule Module {..} = runReader (mempty @InsertedArgsStack) $ do
  _moduleBody' <- checkModuleBody _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        _moduleName,
        _modulePragmas,
        _moduleId
      }

checkModuleBody ::
  (Members '[HighlightBuilder, Reader BuiltinsTable, Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, ResultBuilder, Termination, Reader InsertedArgsStack] r) =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleImports' <- mapM checkImport _moduleImports
  _moduleStatements' <- mapM checkMutualBlock _moduleStatements
  checkModulePositivity _moduleStatements'
  return
    ModuleBody
      { _moduleStatements = _moduleStatements',
        _moduleImports = _moduleImports'
      }

checkModulePositivity ::
  ( Members
      '[ Reader InfoTable,
         Error TypeCheckerError,
         ResultBuilder,
         Reader EntryPoint
       ]
      r
  ) =>
  [MutualBlock] ->
  Sem r ()
checkModulePositivity m = do
  noPos <- asks (^. entryPointNoPositivity)
  checkPositivity noPos m

checkImport :: Import -> Sem r Import
checkImport = return

checkMutualBlock ::
  (HasCallStack, Members '[HighlightBuilder, Reader BuiltinsTable, Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, ResultBuilder, Termination, Reader InsertedArgsStack] r) =>
  MutualBlock ->
  Sem r MutualBlock
checkMutualBlock s = runReader emptyLocalVars (checkTopMutualBlock s)

checkInductiveDef ::
  forall r.
  (Members '[HighlightBuilder, Reader BuiltinsTable, Reader EntryPoint, Reader InfoTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Termination, Output TypedInstanceHole, Output CastHole, Reader InsertedArgsStack, Reader LocalVars] r) =>
  InductiveDef ->
  Sem r InductiveDef
checkInductiveDef InductiveDef {..} = runInferenceDef $ do
  params <- checkParams
  withLocalTypes params $ do
    constrs' <- mapM goConstructor _inductiveConstructors
    ty <- lookupInductiveType _inductiveName
    registerNameIdType (_inductiveName ^. nameId) ty
    inductiveType' <- checkDefType _inductiveType
    let d =
          InductiveDef
            { _inductiveConstructors = constrs',
              _inductiveType = inductiveType',
              _inductiveName,
              _inductiveBuiltin,
              _inductivePositive,
              _inductiveParameters,
              _inductiveTrait,
              _inductivePragmas,
              _inductiveDocComment
            }
    return d
  where
    checkParams :: Sem (Inference ': r) [(Name, Expression)]
    checkParams = execOutputList (go _inductiveParameters)
      where
        go :: [InductiveParameter] -> Sem (Output (Name, Expression) ': Inference ': r) ()
        go = \case
          [] -> return ()
          p : ps -> do
            let pname = p ^. inductiveParamName
            ty' <- checkIsType (getLoc p) (p ^. inductiveParamType)
            output (pname, ty')
            withLocalType pname ty' (go ps)

    goConstructor :: ConstructorDef -> Sem (Inference ': r) ConstructorDef
    goConstructor ConstructorDef {..} = do
      expectedRetTy <- lookupConstructorReturnType _inductiveConstructorName
      cty' <- checkIsType (getLoc _inductiveConstructorType) _inductiveConstructorType
      whenJustM (matchTypes expectedRetTy ret) (const (errRet expectedRetTy))
      let c' =
            ConstructorDef
              { _inductiveConstructorType = cty',
                _inductiveConstructorNormalizedType = Nothing,
                _inductiveConstructorName,
                _inductiveConstructorIsRecord,
                _inductiveConstructorPragmas,
                _inductiveConstructorDocComment
              }
      registerConstructor c'
      return c'
      where
        ret = snd (viewConstructorType _inductiveConstructorType)
        errRet :: Expression -> Sem (Inference ': r) a
        errRet expected =
          throw $
            ErrWrongReturnType
              WrongReturnType
                { _wrongReturnTypeConstructorName = _inductiveConstructorName,
                  _wrongReturnTypeExpected = expected,
                  _wrongReturnTypeActual = ret
                }

checkTopMutualBlock ::
  forall r.
  (HasCallStack, Members '[HighlightBuilder, Reader BuiltinsTable, Reader EntryPoint, Reader LocalVars, Reader InfoTable, Error TypeCheckerError, NameIdGen, ResultBuilder, Termination, Reader InsertedArgsStack] r) =>
  MutualBlock ->
  Sem r MutualBlock
checkTopMutualBlock (MutualBlock ds) =
  MutualBlock
    <$> runInferenceDefs
      ( do
          tys <- sconcatMapM mutualStatementTypes ds
          forM_ tys $ \(name :: Name, ty) -> do
            ty' <- resolveInstanceHoles (resolveCastHoles (checkIsType (getLoc name) ty))
            registerIdenType name ty'
            registerNameIdType (name ^. nameId) ty'
          ls <- mapM checkMutualStatement ds
          putConstructorNormalizedType ls
      )
  where
    putConstructorNormalizedType :: forall r'. (Members '[Inference] r') => NonEmpty MutualStatement -> Sem r' (NonEmpty MutualStatement)
    putConstructorNormalizedType =
      traverseOf
        (each . _StatementInductive . inductiveConstructors . each)
        goConstr
      where
        goConstr :: ConstructorDef -> Sem r' ConstructorDef
        goConstr c = do
          ty' <- strongNormalize (c ^. inductiveConstructorType)
          return (set inductiveConstructorNormalizedType (Just ty') c)

resolveCastHoles ::
  forall a r.
  (Members '[Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Inference, Output TypedInstanceHole, Termination, Reader InsertedArgsStack] r) =>
  Sem (Output CastHole ': r) a ->
  Sem r a
resolveCastHoles s = do
  (hs, e) <- runOutputList s
  -- FIXME this partition is fragile. It assumes that there are only two
  -- CastType's, which may not be true in the future
  let (hs1, hs2) = partition (isCastInt . (^. castHoleType)) hs
  mapM_ (go getIntTy) hs1
  mapM_ (go getNatTy) hs2
  return e
  where
    go :: (Interval -> Sem r Expression) -> CastHole -> Sem r ()
    go mkTy CastHole {..} = do
      m <- queryMetavarFinal _castHoleHole
      case m of
        Just {} -> return ()
        Nothing -> do
          ty <- mkTy (getLoc _castHoleHole)
          void (matchTypes (ExpressionHole _castHoleHole) ty)

mutualStatementTypes :: (Members '[Reader InfoTable] r) => MutualStatement -> Sem r (NonEmpty (Name, Expression))
mutualStatementTypes = \case
  StatementFunction f -> return (pure (f ^. funDefName, f ^. funDefType))
  StatementAxiom f -> return (pure (f ^. axiomName, f ^. axiomType))
  StatementInductive ind -> do
    let indName = ind ^. inductiveName
    indty <- fullInductiveType <$> lookupInductive indName
    ctypes <-
      sequence
        [ do
            let cname = c ^. inductiveConstructorName
            cty <- constructorType <$> lookupConstructor cname
            return (cname, cty)
          | c <- ind ^. inductiveConstructors
        ]
    return $
      (indName, indty)
        :| ctypes

checkMutualStatement ::
  (Members '[HighlightBuilder, Reader BuiltinsTable, Reader EntryPoint, Inference, Reader LocalVars, Reader InfoTable, Error TypeCheckerError, NameIdGen, ResultBuilder, Termination, Reader InsertedArgsStack] r) =>
  MutualStatement ->
  Sem r MutualStatement
checkMutualStatement = \case
  StatementFunction f -> do
    f' <- resolveInstanceHoles (resolveCastHoles (checkFunctionDef f))
    return (StatementFunction f')
  StatementInductive f -> StatementInductive <$> resolveInstanceHoles (resolveCastHoles (checkInductiveDef f))
  StatementAxiom ax -> do
    registerNameIdType (ax ^. axiomName . nameId) (ax ^. axiomType)
    return $ StatementAxiom ax

unfoldFunType1' :: (Members '[Inference] r) => Expression -> Sem r (Maybe (FunctionParameter, Expression))
unfoldFunType1' =
  weakNormalize
    >=> \case
      ExpressionFunction (Function l r) -> return (Just (l, r))
      _ -> return Nothing

unfoldFunType' :: (Members '[Inference] r) => Expression -> Sem r ([FunctionParameter], Expression)
unfoldFunType' e = do
  e' <- unfoldFunType1' e
  case e' of
    Just (l, r) -> first (l :) <$> unfoldFunType' r
    _ -> return ([], e)

checkFunctionDef ::
  forall r.
  (Members '[HighlightBuilder, Reader LocalVars, Reader BuiltinsTable, Reader InfoTable, Error TypeCheckerError, NameIdGen, ResultBuilder, Inference, Termination, Output TypedInstanceHole, Output CastHole, Reader InsertedArgsStack] r) =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  funDef <- do
    _funDefType' <- checkDefType _funDefType
    registerIdenType _funDefName _funDefType'
    registerNameIdType (_funDefName ^. nameId) _funDefType'
    _funDefBody' <- checkFunctionBody _funDefType' _funDefBody
    params <- fst <$> unfoldFunType' _funDefType'
    _funDefArgsInfo' <- checkArgsInfo params
    return
      FunctionDef
        { _funDefBody = _funDefBody',
          _funDefType = _funDefType',
          _funDefArgsInfo = _funDefArgsInfo',
          _funDefName,
          _funDefTerminating,
          _funDefIsInstanceCoercion,
          _funDefBuiltin,
          _funDefPragmas,
          _funDefDocComment
        }
  whenJust _funDefIsInstanceCoercion $ \case
    IsInstanceCoercionCoercion -> checkCoercionType funDef
    IsInstanceCoercionInstance -> checkInstanceType funDef

  registerFunctionDef funDef
  rememberFunctionDef funDef
  return funDef
  where
    -- Since default arguments come from the left of the : then it must be that
    -- there are at least n FunctionParameter
    checkArgsInfo :: [FunctionParameter] -> Sem r [ArgInfo]
    checkArgsInfo allparams = execOutputList $ do
      go (zipExact infos params)
      where
        params = take n allparams
        infos = _funDefArgsInfo
        n = length infos
        go :: [(ArgInfo, FunctionParameter)] -> Sem (Output ArgInfo ': r) ()
        go = \case
          [] -> return ()
          (me, p) : rest -> do
            me' <- traverseOf (argInfoDefault . _Just) (checkExpression (p ^. paramType)) me
            output me'
            withLocalTypeMaybe (p ^. paramName) (p ^. paramType) (go rest)

checkIsType ::
  (HasCallStack, Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Termination, Output TypedInstanceHole, Output CastHole, Reader InsertedArgsStack] r) =>
  Interval ->
  Expression ->
  Sem r Expression
checkIsType loc e =
  checkExpression (smallUniverseE loc) e

checkDefType ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Termination, Output TypedInstanceHole, Output CastHole, Reader InsertedArgsStack] r) =>
  Expression ->
  Sem r Expression
checkDefType ty = checkIsType (getLoc ty) ty

checkInstanceArg ::
  forall r.
  (Members '[Error TypeCheckerError, Reader BuiltinsTable, Reader InfoTable] r) =>
  HashSet VarName ->
  [InstanceParam] ->
  FunctionParameter ->
  Sem r ()
checkInstanceArg metaVars params fp@FunctionParameter {..} = case _paramImplicit of
  Implicit -> return ()
  Explicit -> throw (ErrExplicitInstanceArgument (ExplicitInstanceArgument fp))
  ImplicitInstance -> do
    tab <- ask
    t <- runFail (traitFromExpression metaVars _paramType)
    case t of
      Just app@InstanceApp {..}
        | isTrait tab (_instanceAppHead ^. instanceAppHeadName) ->
            checkTraitTermination app params
      _ ->
        throw (ErrNotATrait (NotATrait _paramType))

checkInstanceArgs ::
  forall r.
  (Members '[Reader BuiltinsTable, Error TypeCheckerError, Reader InfoTable] r) =>
  [FunctionParameter] ->
  [InstanceParam] ->
  Sem r ()
checkInstanceArgs args params = do
  let metaVars = hashSet (mapMaybe (^. paramName) args)
  mapM_ (checkInstanceArg metaVars params) args

checkInstanceType ::
  forall r.
  (Members '[Reader BuiltinsTable, Error TypeCheckerError, Reader InfoTable, Inference, NameIdGen, ResultBuilder] r) =>
  FunctionDef ->
  Sem r ()
checkInstanceType FunctionDef {..} = do
  ty <- strongNormalize _funDefType
  mi <-
    runFail
      . instanceFromTypedIden
      $ TypedIden
        { _typedIdenType = ty ^. normalizedExpression,
          _typedIden = IdenFunction _funDefName
        }
  case mi of
    Just ii@InstanceInfo {..} -> do
      tab <- ask
      unless (isTrait tab _instanceInfoInductive) $
        throw (ErrTargetNotATrait (TargetNotATrait _funDefType))
      itab <- (^. typeCheckingTablesInstanceTable) <$> getCombinedTables
      is <- subsumingInstances itab ii
      unless (null is) $
        throw (ErrSubsumedInstance (SubsumedInstance ii is (getLoc _funDefName)))
      checkInstanceArgs (ii ^. instanceInfoArgs) (ii ^. instanceInfoParams)
      addInstanceInfo ii
    Nothing ->
      throw (ErrInvalidInstanceType (InvalidInstanceType _funDefType))

checkInstanceParam ::
  (Members '[Error TypeCheckerError, Reader BuiltinsTable] r) =>
  InfoTable ->
  NormalizedExpression ->
  Sem r ()
checkInstanceParam tab ty' = do
  t <- runFail (traitFromExpression mempty ty)
  case t of
    Just InstanceApp {..}
      | isTrait tab (_instanceAppHead ^. instanceAppHeadName) -> return ()
    _ -> throw (ErrNotATrait (NotATrait ty))
  where
    ty = ty' ^. normalizedExpression

checkCoercionType ::
  forall r.
  (Members '[Reader BuiltinsTable, Error TypeCheckerError, Reader InfoTable, Inference, ResultBuilder] r) =>
  FunctionDef ->
  Sem r ()
checkCoercionType FunctionDef {..} = do
  ty <- strongNormalize _funDefType
  mi <-
    runFail $
      coercionFromTypedIden
        TypedIden
          { _typedIdenType = ty ^. normalizedExpression,
            _typedIden = IdenFunction _funDefName
          }
  case mi of
    Just ci@CoercionInfo {..} -> do
      tab <- ask
      unless (isTrait tab _coercionInfoInductive) $
        throw (ErrTargetNotATrait (TargetNotATrait _funDefType))
      unless (isTrait tab (_coercionInfoTarget ^. instanceAppHead . instanceAppHeadName)) $
        throw (ErrInvalidCoercionType (InvalidCoercionType _funDefType))
      checkInstanceArgs (ci ^. coercionInfoArgs) (ci ^. coercionInfoParams)
      addCoercionInfo (checkCoercionInfo ci)
      checkCoercionCycles
    Nothing ->
      throw (ErrInvalidCoercionType (InvalidCoercionType _funDefType))

checkCaseBranchRhs ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Output TypedInstanceHole, Termination, Output CastHole, Reader InsertedArgsStack] r) =>
  Expression ->
  CaseBranchRhs ->
  Sem r CaseBranchRhs
checkCaseBranchRhs expectedTy = \case
  CaseBranchRhsExpression e -> CaseBranchRhsExpression <$> checkExpression expectedTy e
  CaseBranchRhsIf s -> CaseBranchRhsIf <$> checkSideIfs expectedTy s

checkSideIfs ::
  forall r.
  ( Members
      '[ Reader InfoTable,
         ResultBuilder,
         Error TypeCheckerError,
         NameIdGen,
         Reader LocalVars,
         Inference,
         Output TypedInstanceHole,
         Termination,
         HighlightBuilder,
         Reader BuiltinsTable,
         Output CastHole,
         Reader InsertedArgsStack
       ]
      r
  ) =>
  Expression ->
  SideIfs ->
  Sem r SideIfs
checkSideIfs expectedTy SideIfs {..} = do
  branches' <- mapM (checkSideIfBranch expectedTy) _sideIfBranches
  else' <- mapM (checkExpression expectedTy) _sideIfElse
  return
    SideIfs
      { _sideIfBranches = branches',
        _sideIfElse = else'
      }

checkSideIfBranch ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Output TypedInstanceHole, Termination, Output CastHole, Reader InsertedArgsStack] r) =>
  Expression ->
  SideIfBranch ->
  Sem r SideIfBranch
checkSideIfBranch expectedTy SideIfBranch {..} = do
  boolTy <- getBoolType (getLoc _sideIfBranchCondition)
  cond' <- checkExpression boolTy _sideIfBranchCondition
  body' <- checkExpression expectedTy _sideIfBranchBody
  return
    SideIfBranch
      { _sideIfBranchCondition = cond',
        _sideIfBranchBody = body'
      }

getBoolType :: (Members '[Reader InfoTable, Reader BuiltinsTable, Error TypeCheckerError] r) => Interval -> Sem r Expression
getBoolType loc = toExpression <$> getBuiltinNameTypeChecker loc BuiltinBool

checkExpression ::
  forall r.
  (HasCallStack, Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Output TypedInstanceHole, Termination, Output CastHole, Reader InsertedArgsStack] r) =>
  Expression ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  let hint =
        TypeHint
          { _typeHint = Just expectedTy
          }
  e' <- inferExpression' hint e
  let inferredType = e' ^. typedType
  whenJustM (matchTypes expectedTy inferredType) (const (err e'))
  return (e' ^. typedExpression)
  where
    err :: TypedExpression -> Sem r a
    err inferred = do
      e' <- strongNormalize e
      inferred' <- strongNormalize (inferred ^. typedType)
      expected' <- strongNormalize expectedTy
      let thing =
            WrongTypeThingExpression
              MkWrongTypeThingExpression
                { _wrongTypeNormalizedExpression = e',
                  _wrongTypeInferredExpression = inferred ^. typedExpression
                }
      throw
        . ErrWrongType
        $ WrongType
          { _wrongTypeThing = thing,
            _wrongTypeActual = inferred',
            _wrongTypeExpected = expected'
          }

resolveInstanceHoles ::
  forall a r.
  ( HasExpressions a,
    Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Inference, Termination, Reader InsertedArgsStack] r
  ) =>
  Sem (Output TypedInstanceHole ': r) a ->
  Sem r a
resolveInstanceHoles s = do
  (hs :: [TypedInstanceHole], e) <- runOutputList s
  subs :: HashMap InstanceHole Expression <-
    hashMap
      <$> sequence
        [ do
            h' <- goResolve h
            return (h ^. typedInstanceHoleHole, h')
          | h <- hs
        ]
  subsInstanceHoles subs e
  where
    -- Recursively resolve an instance hole
    goResolve :: TypedInstanceHole -> Sem r Expression
    goResolve h0 = do
      -- Needed to insert instance holes (e.g. for Natural literals)
      h@TypedInstanceHole {..} <-
        resolveInstanceHoles
          . resolveCastHoles
          . runReader (h0 ^. typedInstanceHoleLocalVars)
          $ typedInstanceHoleType
            ( \t -> do
                t0 <- (^. normalizedExpression) <$> strongNormalize t
                (^. typedExpression) <$> inferExpression' emptyTypeHint t0
            )
            h0

      t <- resolveTraitInstance h
      resolveInstanceHoles
        . resolveCastHoles
        . runReader _typedInstanceHoleLocalVars
        $ checkExpression _typedInstanceHoleType t

checkFunctionParameter ::
  (HasCallStack, Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Termination, Output TypedInstanceHole, Output CastHole, Reader InsertedArgsStack] r) =>
  FunctionParameter ->
  Sem r FunctionParameter
checkFunctionParameter FunctionParameter {..} = do
  ty <- checkIsType (getLoc _paramType) _paramType
  ty' <- strongNormalize ty
  when (_paramImplicit == ImplicitInstance) $ do
    tab <- ask
    checkInstanceParam tab ty'
  return
    FunctionParameter
      { _paramType = ty' ^. normalizedExpression,
        _paramName,
        _paramImplicit
      }

inferExpressionRepl ::
  (Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Termination, Reader InsertedArgsStack] r) =>
  -- | type hint
  Maybe Expression ->
  Expression ->
  Sem r TypedExpression
inferExpressionRepl hint = resolveInstanceHoles . resolveCastHoles . inferExpression' (mkTypeHint hint)

lookupVar ::
  (HasCallStack) =>
  (Members '[Reader LocalVars, Reader InfoTable] r) =>
  Name ->
  Sem r Expression
lookupVar v = do
  locals <- asks (^. localTypes)
  return $ fromMaybe err (locals ^. at v)
  where
    err = error $ "internal error: could not find var " <> ppTrace v <> " at " <> ppTrace (getLoc v)

checkFunctionBody ::
  (Members '[HighlightBuilder, Reader LocalVars, Reader BuiltinsTable, Reader InfoTable, NameIdGen, Error TypeCheckerError, Output TypedInstanceHole, ResultBuilder, Inference, Termination, Output CastHole, Reader InsertedArgsStack] r) =>
  Expression ->
  Expression ->
  Sem r Expression
checkFunctionBody expectedTy body =
  case body of
    ExpressionLambda {} -> checkExpression expectedTy body
    _ -> do
      (patterns', typedBody) <- checkClauseExpression (getLoc body) expectedTy [] body
      return $ case nonEmpty patterns' of
        Nothing -> typedBody
        Just lambdaPatterns' ->
          ExpressionLambda
            Lambda
              { _lambdaType = Nothing,
                _lambdaClauses =
                  pure
                    LambdaClause
                      { _lambdaPatterns = lambdaPatterns',
                        _lambdaBody = typedBody
                      }
              }

checkClauseExpression ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Termination, Output TypedInstanceHole, Output CastHole, Reader InsertedArgsStack] r) =>
  Interval ->
  -- | Type
  Expression ->
  -- | Arguments
  [PatternArg] ->
  -- | Body
  Expression ->
  Sem r ([PatternArg], Expression) -- (Checked patterns, Checked body)
checkClauseExpression clauseLoc clauseType clausePats body = do
  (pats', rhs') <- checkClause clauseLoc clauseType clausePats (CaseBranchRhsExpression body)
  case rhs' of
    CaseBranchRhsExpression body' -> return (pats', body')
    CaseBranchRhsIf {} -> impossible

-- | helper function for lambda functions and case branches
checkClause ::
  forall r.
  (Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Termination, Output TypedInstanceHole, Output CastHole, Reader InsertedArgsStack] r) =>
  Interval ->
  -- | Type
  Expression ->
  -- | Arguments
  [PatternArg] ->
  -- | Body
  CaseBranchRhs ->
  Sem r ([PatternArg], CaseBranchRhs) -- (Checked patterns, Checked body)
checkClause clauseLoc clauseType clausePats body = do
  locals0 <- ask
  (localsPats, (checkedPatterns, bodyType)) <- helper clausePats clauseType
  let locals' = locals0 <> localsPats
  bodyTy' <- substitutionE (localsToSubsE locals') bodyType
  checkedBody <- local (const locals') (checkCaseBranchRhs bodyTy' body)
  return (checkedPatterns, checkedBody)
  where
    helper :: [PatternArg] -> Expression -> Sem r (LocalVars, ([PatternArg], Expression))
    helper pats ty = runState emptyLocalVars (go pats ty)

    genPatternWildcard :: Interval -> FunctionParameter -> Sem (State LocalVars ': r) PatternArg
    genPatternWildcard loc par = do
      let impl = par ^. paramImplicit
      var <- maybe (varFromWildcard (Wildcard loc)) return (par ^. paramName)
      addPatternVar var (par ^. paramType) Nothing
      return
        PatternArg
          { _patternArgIsImplicit = impl,
            _patternArgName = Nothing,
            _patternArgPattern = PatternVariable var
          }

    go :: [PatternArg] -> Expression -> Sem (State LocalVars ': r) ([PatternArg], Expression)
    go pats bodyTy = case pats of
      [] -> do
        (bodyParams, bodyRest) <- unfoldFunType' bodyTy
        locals <- get
        guessedBodyParams <- withLocalVars locals (unfoldArity <$> arityCaseRhs body)
        let pref' :: [FunctionParameter] = take pref bodyParams
            pref :: Int = aI - targetI
            preImplicits = length . takeWhile isImplicitOrInstance
            aI :: Int = preImplicits (map (^. paramImplicit) bodyParams)
            targetI :: Int = preImplicits (map (^. arityParameterImplicit) guessedBodyParams)
        if
            | 0 < pref -> do
                let n = length pref'
                    bodyParams' = drop n bodyParams
                    ty' = foldFunType bodyParams' bodyRest
                wildcards <- mapM (genPatternWildcard clauseLoc) pref'
                return (wildcards, ty')
            | otherwise -> return ([], bodyTy)
      p : ps -> do
        bodyTy' <- weakNormalize bodyTy
        case bodyTy' of
          ExpressionHole h -> do
            fun <- holeRefineToFunction (p ^. patternArgIsImplicit) h
            go pats (ExpressionFunction fun)
          _ -> do
            unfoldedBodyTy' <- unfoldFunType' bodyTy'
            case unfoldedBodyTy' of
              ([], _) -> throwTooManyPatterns
              (par : pars, ret) -> do
                let checkPatternAndContinue = do
                      par' <- checkPattern par p
                      first (par' :) <$> go ps (foldFunType pars ret)

                    loc :: Interval
                    loc = getLoc par

                    insertWildcard :: Sem (State LocalVars ': r) ([PatternArg], Expression)
                    insertWildcard = do
                      w <- genPatternWildcard loc par
                      go (w : p : ps) bodyTy'

                case (p ^. patternArgIsImplicit, par ^. paramImplicit) of
                  (Explicit, Explicit) -> checkPatternAndContinue
                  (Implicit, Implicit) -> checkPatternAndContinue
                  (ImplicitInstance, ImplicitInstance) -> checkPatternAndContinue
                  (Implicit, Explicit) -> throwWrongIsImplicit p Implicit
                  (ImplicitInstance, Explicit) -> throwWrongIsImplicit p ImplicitInstance
                  (Explicit, Implicit) -> insertWildcard
                  (ImplicitInstance, Implicit) -> insertWildcard
                  (Explicit, ImplicitInstance) -> insertWildcard
                  (Implicit, ImplicitInstance) -> insertWildcard
        where
          throwWrongIsImplicit :: (Members '[Error TypeCheckerError] r') => PatternArg -> IsImplicit -> Sem r' a
          throwWrongIsImplicit patArg expected =
            throw . ErrArityCheckerError $
              ErrWrongPatternIsImplicit
                WrongPatternIsImplicit
                  { _wrongPatternIsImplicitActual = patArg,
                    _wrongPatternIsImplicitExpected = expected
                  }
          throwTooManyPatterns :: (Members '[Error TypeCheckerError] r') => Sem r' a
          throwTooManyPatterns =
            throw . ErrArityCheckerError $
              ErrLhsTooManyPatterns
                LhsTooManyPatterns
                  { _lhsTooManyPatternsRemaining = p :| ps
                  }

freshHoleImpl :: (Members '[NameIdGen] r) => Interval -> IsImplicit -> Sem r Expression
freshHoleImpl loc = \case
  Explicit -> ExpressionHole <$> Extra.freshHole loc
  Implicit -> ExpressionHole <$> Extra.freshHole loc
  ImplicitInstance -> ExpressionInstanceHole <$> Extra.freshInstanceHole loc

-- | Refines a hole into a function type. I.e. '_@1' is matched with '_@fresh → _@fresh'
holeRefineToFunction :: (Members '[Inference, NameIdGen] r) => IsImplicit -> Hole -> Sem r Function
holeRefineToFunction impl h = do
  s <- queryMetavar h
  case s of
    Just h' -> case h' of
      ExpressionFunction f -> return f
      ExpressionHole h'' -> holeRefineToFunction impl h''
      _ -> error "cannot refine hole to function"
    Nothing -> do
      l <- freshHoleImpl (getLoc h) impl
      r <- freshHoleImpl (getLoc h) Implicit
      let fun = Function (unnamedParameter' impl l) r
      whenJustM (matchTypes (ExpressionHole h) (ExpressionFunction fun)) impossible
      return fun

matchIsImplicit :: (Member (Error TypeCheckerError) r) => IsImplicit -> PatternArg -> Sem r ()
matchIsImplicit expected actual =
  unless
    (expected == actual ^. patternArgIsImplicit)
    . throw
    . ErrArityCheckerError
    $ ErrWrongPatternIsImplicit
      WrongPatternIsImplicit
        { _wrongPatternIsImplicitExpected = expected,
          _wrongPatternIsImplicitActual = actual
        }

addPatternVar :: (Members '[State LocalVars, Inference] r) => VarName -> Expression -> Maybe Name -> Sem r ()
addPatternVar v ty argName = do
  modify (addType v ty)
  registerIdenType v ty
  whenJust argName (\v' -> modify (addTypeMapping v' v))

checkPattern ::
  forall r.
  (Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars, Inference, NameIdGen, ResultBuilder] r) =>
  FunctionParameter ->
  PatternArg ->
  Sem r PatternArg
checkPattern = go
  where
    go :: FunctionParameter -> PatternArg -> Sem r PatternArg
    go argTy patArg = do
      matchIsImplicit (argTy ^. paramImplicit) patArg
      tyVarMap <- fmap (ExpressionIden . IdenVar) . (^. localTyMap) <$> get
      ty <- substitutionE tyVarMap (argTy ^. paramType)
      let pat = patArg ^. patternArgPattern
          name = patArg ^. patternArgName
      whenJust name (\n -> addPatternVar n ty (argTy ^. paramName))
      pat' <- case pat of
        PatternVariable v -> addPatternVar v ty (argTy ^. paramName) $> pat
        PatternWildcardConstructor c -> checkWildcardConstructor pat ty c
        PatternConstructorApp a -> goPatternConstructor pat ty a
      return (set patternArgPattern pat' patArg)
      where
        checkWildcardConstructor :: Pattern -> Expression -> WildcardConstructor -> Sem r Pattern
        checkWildcardConstructor pat ty w = do
          let c = w ^. wildcardConstructor
          numArgs <- length . constructorArgs . (^. constructorInfoType) <$> lookupConstructor c
          holeArgs <- replicateM numArgs (genWildcard (getLoc w) Explicit)
          let capp =
                ConstructorApp
                  { _constrAppConstructor = c,
                    _constrAppParameters = holeArgs,
                    _constrAppType = Nothing
                  }
          goPatternConstructor pat ty capp

        goPatternConstructor :: Pattern -> Expression -> ConstructorApp -> Sem r Pattern
        goPatternConstructor pat ty a = do
          s <- checkSaturatedInductive ty
          info <- lookupConstructor (a ^. constrAppConstructor)
          let constrIndName = info ^. constructorInfoInductive
              constrName = a ^. constrAppConstructor
              err :: MatchError -> Sem r ()
              err m =
                throw $
                  ErrWrongType
                    WrongType
                      { _wrongTypeThing = WrongTypeThingPattern pat,
                        _wrongTypeExpected = m ^. matchErrorRight,
                        _wrongTypeActual = m ^. matchErrorLeft
                      }
          case s of
            Left hole -> do
              let indParams = info ^. constructorInfoInductiveParameters
                  numIndParams = length indParams
                  indName :: Iden
                  indName = IdenInductive (info ^. constructorInfoInductive)
                  loc = getLoc a
              paramHoles <- replicateM numIndParams (freshHoleImpl loc Implicit)
              let patternTy = foldApplication (ExpressionIden indName) (map (ApplicationArg Explicit) paramHoles)
              whenJustM
                (matchTypes patternTy (ExpressionHole hole))
                err
              let tyArgs = zipExact indParams paramHoles
              PatternConstructorApp <$> goConstr indName a tyArgs
            Right (ind, tyArgs) -> do
              when
                (ind /= constrIndName)
                $ throw
                $ ErrWrongConstructorType
                  WrongConstructorType
                    { _wrongCtorTypeName = constrName,
                      _wrongCtorTypeExpected = ind,
                      _wrongCtorTypeActual = constrIndName
                    }
              PatternConstructorApp <$> goConstr (IdenInductive ind) a tyArgs

        goConstr :: Iden -> ConstructorApp -> [(InductiveParameter, Expression)] -> Sem r ConstructorApp
        goConstr inductivename app@(ConstructorApp c ps _) ctx = do
          (_, psTys) <- constructorArgTypes <$> lookupConstructor c
          psTys' <- mapM (substituteIndParams ctx) psTys
          let expectedNum = length psTys
              w :: [FunctionParameter] = psTys'
          when (expectedNum /= length ps) (throw (appErr app expectedNum))
          pis <- zipWithM go w ps
          let appTy = foldExplicitApplication (ExpressionIden inductivename) (map snd ctx)
          return app {_constrAppType = Just appTy, _constrAppParameters = pis}

        appErr :: ConstructorApp -> Int -> TypeCheckerError
        appErr app expected =
          ErrArityCheckerError $
            ErrWrongConstructorAppLength
              WrongConstructorAppLength
                { _wrongConstructorAppLength = app,
                  _wrongConstructorAppLengthExpected = expected
                }

    checkSaturatedInductive :: Expression -> Sem r (Either Hole (InductiveName, [(InductiveParameter, Expression)]))
    checkSaturatedInductive ty = do
      i <- viewInductiveApp ty
      case i of
        Left hole -> return (Left hole)
        Right (ind, args) -> do
          params :: [InductiveParameter] <-
            (^. inductiveInfoParameters)
              <$> lookupInductive ind
          let numArgs = length args
              numParams = length params
          when
            (numArgs < numParams)
            ( throw $
                ErrTooFewArgumentsIndType
                  WrongNumberArgumentsIndType
                    { _wrongNumberArgumentsIndTypeActualType = ty,
                      _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                      _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                    }
            )
          when
            (numArgs > numParams)
            ( throw $
                ErrTooManyArgumentsIndType
                  WrongNumberArgumentsIndType
                    { _wrongNumberArgumentsIndTypeActualType = ty,
                      _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                      _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                    }
            )
          return (Right (ind, zipExact params args))

inferExpression' ::
  forall r.
  (HasCallStack, Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Output TypedInstanceHole, Termination, Output CastHole, Reader InsertedArgsStack, Reader InsertedArgsStack] r) =>
  TypeHint ->
  Expression ->
  Sem r TypedExpression
inferExpression' = holesHelper

-- | Checks anything but an Application. Does not insert holes
inferLeftAppExpression ::
  forall r.
  ( HasCallStack,
    Members
      '[ Reader InfoTable,
         Reader BuiltinsTable,
         HighlightBuilder,
         ResultBuilder,
         Reader LocalVars,
         Error TypeCheckerError,
         NameIdGen,
         Inference,
         Output TypedInstanceHole,
         Termination,
         Output CastHole,
         HighlightBuilder,
         Reader InsertedArgsStack
       ]
      r
  ) =>
  TypeHint ->
  Expression ->
  Sem r TypedExpression
inferLeftAppExpression mhint e = case e of
  ExpressionApplication {} -> impossible
  ExpressionIden i -> idenType i
  ExpressionLiteral l -> goLiteral l
  ExpressionNatural n -> goNatural n
  ExpressionFunction f -> goFunction f
  ExpressionHole h -> goHole h
  ExpressionInstanceHole h -> goInstanceHole h
  ExpressionUniverse u -> goUniverse u
  ExpressionSimpleLambda l -> goSimpleLambda l
  ExpressionLambda l -> goLambda l
  ExpressionLet l -> goLet l
  ExpressionCase l -> goCase l
  where
    goNatural :: BuiltinNatural -> Sem r TypedExpression
    goNatural n = do
      natTy <- toExpression <$> getBuiltinNameTypeChecker (getLoc n) BuiltinNat
      return
        TypedExpression
          { _typedType = natTy,
            _typedExpression = ExpressionNatural n
          }

    goLet :: Let -> Sem r TypedExpression
    goLet l = do
      _letClauses <- mapM goLetClause (l ^. letClauses)
      typedBody <- inferExpression' mhint (l ^. letExpression)
      return
        TypedExpression
          { _typedType = typedBody ^. typedType,
            _typedExpression =
              ExpressionLet
                Let
                  { _letExpression = typedBody ^. typedExpression,
                    _letClauses
                  }
          }

    goLetClause :: LetClause -> Sem r LetClause
    goLetClause = \case
      LetFunDef f -> LetFunDef <$> checkFunctionDef f
      LetMutualBlock b -> LetMutualBlock <$> goMutualLet b
      where
        goMutualLet :: MutualBlockLet -> Sem r MutualBlockLet
        goMutualLet (MutualBlockLet fs) = MutualBlockLet <$> mapM checkFunctionDef fs

    goHole :: Hole -> Sem r TypedExpression
    goHole h = do
      void (queryMetavar h)
      ty <- maybe (freshHoleImpl (getLoc h) Implicit) return (mhint ^. typeHint)
      return
        TypedExpression
          { _typedExpression = ExpressionHole h,
            _typedType = ty
          }

    goInstanceHole :: InstanceHole -> Sem r TypedExpression
    goInstanceHole h = do
      let ty = fromMaybe impossible (mhint ^. typeHint)
      locals <- ask
      output (TypedInstanceHole h ty locals)
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionInstanceHole h
          }

    goSimpleLambda :: SimpleLambda -> Sem r TypedExpression
    goSimpleLambda (SimpleLambda (SimpleBinder v ty) b) = do
      b' <- withLocalType v ty (inferExpression' emptyTypeHint b)
      let smallUni = smallUniverseE (getLoc ty)
      ty' <- checkExpression smallUni ty
      let fun = Function (unnamedParameter smallUni) (b' ^. typedType)
      return
        TypedExpression
          { _typedType = ExpressionFunction fun,
            _typedExpression = ExpressionSimpleLambda (SimpleLambda (SimpleBinder v ty') (b' ^. typedExpression))
          }

    goCase :: Case -> Sem r TypedExpression
    goCase c = do
      ty <- case mhint ^. typeHint of
        Nothing -> freshHoleImpl (getLoc c) Implicit
        Just hi -> return hi
      typedCaseExpression <- inferExpression' emptyTypeHint (c ^. caseExpression)
      let _caseExpression = typedCaseExpression ^. typedExpression
          _caseExpressionType = Just (typedCaseExpression ^. typedType)
          _caseExpressionWholeType = Just ty
          goBranch :: CaseBranch -> Sem r CaseBranch
          goBranch b = do
            (onePat, _caseBranchRhs) <-
              checkClause
                (getLoc b)
                funty
                [b ^. caseBranchPattern]
                (b ^. caseBranchRhs)
            let _caseBranchPattern = case onePat of
                  [x] -> x
                  _ -> impossible
            return CaseBranch {..}
            where
              funty :: Expression
              funty = ExpressionFunction (mkFunction (typedCaseExpression ^. typedType) ty)
      _caseBranches <- mapM goBranch (c ^. caseBranches)
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionCase Case {..}
          }

    goLambda :: Lambda -> Sem r TypedExpression
    goLambda l = do
      ty <- case mhint ^. typeHint of
        Just hi -> return hi
        Nothing -> freshHoleImpl (getLoc l) Implicit
      _lambdaClauses <- mapM (goClause ty) (l ^. lambdaClauses)
      let lty' = Just ty
          l' =
            Lambda
              { _lambdaType = lty',
                _lambdaClauses
              }
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionLambda l'
          }
      where
        goClause :: Expression -> LambdaClause -> Sem r LambdaClause
        goClause ty cl@LambdaClause {..} = do
          (pats', body') <- checkClauseExpression (getLoc cl) ty (toList _lambdaPatterns) _lambdaBody
          return
            LambdaClause
              { _lambdaPatterns = nonEmpty' pats',
                _lambdaBody = body'
              }

    goUniverse :: SmallUniverse -> Sem r TypedExpression
    goUniverse u =
      return
        TypedExpression
          { _typedType = ExpressionUniverse u,
            _typedExpression = ExpressionUniverse u
          }

    goFunction :: Function -> Sem r TypedExpression
    goFunction (Function l r) = do
      let uni = smallUniverseE (getLoc l)
      l' <- checkFunctionParameter l
      let bodyEnv :: Sem r a -> Sem r a
          bodyEnv = withLocalTypeMaybe (l ^. paramName) (l ^. paramType)
      r' <- bodyEnv (checkExpression uni r)
      return (TypedExpression uni (ExpressionFunction (Function l' r')))

    goLiteral :: LiteralLoc -> Sem r TypedExpression
    goLiteral lit@(WithLoc i l) = case l of
      LitNumeric v -> typedLitNumeric v
      LitInteger {} -> do
        ty <- getIntTy i
        return $
          TypedExpression
            { _typedType = ty,
              _typedExpression = ExpressionLiteral lit
            }
      LitNatural {} -> do
        ty <- getNatTy i
        return $
          TypedExpression
            { _typedType = ty,
              _typedExpression = ExpressionLiteral lit
            }
      LitString {} -> do
        str <- getBuiltinNameTypeChecker i BuiltinString
        return
          TypedExpression
            { _typedExpression = ExpressionLiteral lit,
              _typedType = ExpressionIden (IdenAxiom str)
            }
      where
        typedLitNumeric :: Integer -> Sem r TypedExpression
        typedLitNumeric v = do
          castHole v
          if
              | v < 0 -> getIntTy i >>= typedLit LitInteger BuiltinFromInt
              | otherwise -> getNatTy i >>= typedLit (LitNatural . fromInteger) BuiltinFromNat
          where
            typedLit :: (Integer -> Literal) -> BuiltinFunction -> Expression -> Sem r TypedExpression
            typedLit litt blt ty = do
              from <- getBuiltinNameTypeChecker i blt
              ihole <- freshHoleImpl i ImplicitInstance
              let ty' = maybe ty (adjustLocation i) (mhint ^. typeHint)
              inferExpression' (mkTypeHint (Just ty')) $
                foldApplication
                  (ExpressionIden (IdenFunction from))
                  [ ApplicationArg Implicit ty',
                    ApplicationArg ImplicitInstance ihole,
                    ApplicationArg Explicit (ExpressionLiteral (WithLoc i (litt v)))
                  ]

        castHole :: Integer -> Sem r ()
        castHole v =
          case mhint ^. typeHint of
            Just (ExpressionHole h) ->
              let outCastHole ty =
                    output
                      CastHole
                        { _castHoleHole = h,
                          _castHoleType = ty
                        }
               in if
                      | v < 0 -> outCastHole CastInt
                      | otherwise -> outCastHole CastNat
            _ -> return ()

idenType :: forall r. (HasCallStack, Members '[Reader LocalVars, Reader InfoTable, ResultBuilder] r) => Iden -> Sem r TypedExpression
idenType i = case i of
  IdenVar v -> do
    ty <- lookupVar v
    return (TypedExpression ty (ExpressionIden i))
  IdenFunction fun ->
    goDefIden fun `runFailOrM` do
      info <- lookupFunction fun
      return (TypedExpression (info ^. functionInfoType) (ExpressionIden i))
  IdenConstructor c ->
    goDefIden c `runFailOrM` do
      ty <- lookupConstructorType c
      return (TypedExpression ty (ExpressionIden i))
  IdenAxiom a ->
    goDefIden a `runFailOrM` do
      info <- lookupAxiom a
      return (TypedExpression (info ^. axiomInfoDef . axiomType) (ExpressionIden i))
  IdenInductive d ->
    goDefIden d `runFailOrM` do
      kind <- lookupInductiveType d
      return (TypedExpression kind (ExpressionIden i))
  where
    -- We get the type from the ResultBuilder if available, as it may have
    -- been further refined. If not present, we fallback to the InfoTable
    goDefIden :: Name -> Sem (Fail ': r) TypedExpression
    goDefIden name = do
      ty <- lookupIdenType (name ^. nameId) >>= failMaybe
      return (TypedExpression ty (ExpressionIden i))

-- | The _typeHint is used for trailing holes only
holesHelper :: forall r. (HasCallStack, Members '[HighlightBuilder, Reader InfoTable, Reader BuiltinsTable, ResultBuilder, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Output TypedInstanceHole, Termination, Output CastHole, Reader InsertedArgsStack] r) => TypeHint -> Expression -> Sem r TypedExpression
holesHelper mhint expr = do
  let (f, args) = unfoldExpressionApp expr
      hint
        | null args = mhint
        | otherwise = emptyTypeHint
  arityCheckBuiltins f args
  fTy <- inferLeftAppExpression hint f

  iniBuilderType <- mkInitBuilderType fTy
  let iniArg :: ApplicationArg -> AppBuilderArg
      iniArg a =
        AppBuilderArg
          { _appBuilderArgIsDefault = ItIsNotDefault,
            _appBuilderArg = a
          }
      iniBuilder =
        AppBuilder
          { _appBuilderLeft = fTy ^. typedExpression,
            _appBuilderType = iniBuilderType,
            _appBuilderTypeCtx = mempty,
            _appBuilderArgs = map iniArg args
          }
  (insertedArgs, st') <- runOutputList (execState iniBuilder goAllArgs)
  let ty' = mkFinalBuilderType (st' ^. appBuilderType)
      expr' = mkFinalExpression (st' ^. appBuilderLeft) insertedArgs
  return
    TypedExpression
      { _typedType = ty',
        _typedExpression = expr'
      }
  where
    mkArg :: InsertedArg -> ApplicationArg
    mkArg i =
      ApplicationArg
        { _appArg = i ^. insertedValue,
          _appArgIsImplicit = i ^. insertedImplicit
        }
    mkFinalExpression :: Expression -> [InsertedArg] -> Expression
    mkFinalExpression f args = foldApplication f (map mkArg args)

    mkFinalBuilderType :: BuilderType -> Expression
    mkFinalBuilderType = \case
      BuilderTypeNoDefaults e -> e
      BuilderTypeDefaults f ->
        ExpressionFunction
          Function
            { _functionLeft = f ^. functionDefaultLeft,
              _functionRight = mkFinalBuilderType (f ^. functionDefaultRight)
            }

    extendCtx :: (Members '[State AppBuilder] r') => FunctionParameter -> Expression -> Sem r' ()
    extendCtx funParam arg' = whenJust (funParam ^. paramName) $ \nm -> do
      modify' (over appBuilderTypeCtx (set (at nm) (Just arg')))

    applyCtx :: (Members '[State AppBuilder, NameIdGen] r', HasExpressions exp) => exp -> Sem r' exp
    applyCtx x = do
      s <- gets (^. appBuilderTypeCtx)
      substitutionE s x

    getFunctionName :: Expression -> Maybe Name
    getFunctionName = \case
      ExpressionIden (IdenFunction fun) -> Just fun
      _ -> Nothing

    mkInitBuilderType :: TypedExpression -> Sem r BuilderType
    mkInitBuilderType fTy =
      let ty = fTy ^. typedType
       in runFailDefault (BuilderTypeNoDefaults ty) $ do
            fun <- failMaybe (getFunctionName (fTy ^. typedExpression))
            infos <- (^. functionInfoArgsInfo) <$> lookupFunction fun
            return $ toFunctionDefaultMay fun ty infos
      where
        toFunctionDefaultMay :: Name -> Expression -> [ArgInfo] -> BuilderType
        toFunctionDefaultMay funName ty infos =
          let ixInfos = nonEmpty (indexFrom 0 infos)
           in maybe (BuilderTypeNoDefaults ty) (BuilderTypeDefaults . toFunctionDefault funName ty) ixInfos

        toFunctionDefault :: Name -> Expression -> NonEmpty (Indexed ArgInfo) -> FunctionDefault
        toFunctionDefault _argIdFunctionName e (Indexed _argIdIx a :| as) = case e of
          ExpressionFunction f ->
            let r' =
                  toFunctionDefaultMay
                    _argIdFunctionName
                    (f ^. functionRight)
                    (map (^. indexedThing) as)
             in FunctionDefault
                  { _functionDefaultLeft = f ^. functionLeft,
                    _functionDefaultRight = r',
                    _functionDefaultDefault =
                      let uid =
                            ArgId
                              { _argIdDefinitionLoc = Irrelevant (getLoc f),
                                _argIdName = Irrelevant (a ^. argInfoName),
                                _argIdFunctionName,
                                _argIdIx
                              }
                       in FunctionDefaultInfo uid <$> a ^. argInfoDefault
                  }
          _ -> impossible

    arityCheckBuiltins :: Expression -> [ApplicationArg] -> Sem r ()
    arityCheckBuiltins f args = do
      case f of
        ExpressionIden (IdenAxiom n) -> do
          blt <- getAxiomBuiltinInfo n
          case blt of
            Just BuiltinIOSequence -> checkBuiltinApp n 0 2 args
            _ -> return ()
        ExpressionIden (IdenFunction n) -> do
          blt <- getFunctionBuiltinInfo n
          case blt of
            Just BuiltinBoolIf -> checkBuiltinApp n 1 3 args
            Just BuiltinBoolOr -> checkBuiltinApp n 0 2 args
            Just BuiltinBoolAnd -> checkBuiltinApp n 0 2 args
            Just BuiltinSeq -> checkBuiltinApp n 2 2 args
            Just BuiltinAssert -> checkBuiltinApp n 0 1 args
            _ -> return ()
        _ -> return ()

    checkBuiltinApp :: Name -> Int -> Int -> [ApplicationArg] -> Sem r ()
    checkBuiltinApp n implArgsNum argsNum args = do
      args' <- goImplArgs implArgsNum args
      if
          | length args' >= argsNum -> return ()
          | otherwise ->
              throw
                . ErrArityCheckerError
                $ ErrBuiltinNotFullyApplied
                  BuiltinNotFullyApplied
                    { _builtinNotFullyAppliedName = n,
                      _builtinNotFullyAplliedExpectedArgsNum = argsNum
                    }
      where
        goImplArgs :: Int -> [ApplicationArg] -> Sem r [ApplicationArg]
        goImplArgs 0 as = return as
        goImplArgs k (ApplicationArg Implicit _ : as) = goImplArgs (k - 1) as
        goImplArgs _ as = return as

    goAllArgs :: forall r'. (r' ~ State AppBuilder ': Output InsertedArg ': r) => Sem r' ()
    goAllArgs = do
      goArgs
      gets (^. appBuilderType) >>= applyCtx >>= modify' . set appBuilderType

    goArgs :: forall r'. (r' ~ State AppBuilder ': Output InsertedArg ': r) => Sem r' ()
    goArgs = peekArg >>= maybe (insertTrailingHolesMay (mhint ^. typeHint)) goNextArg
      where
        insertTrailingHolesMay :: Maybe Expression -> Sem r' ()
        insertTrailingHolesMay = flip whenJust insertTrailingHoles

        insertTrailingHoles :: Expression -> Sem r' ()
        insertTrailingHoles hintTy = do
          builderTy <- gets (^. appBuilderType) >>= applyCtx
          ariHint <- typeArity hintTy
          let (defaults, restExprTy) = peelDefault builderTy
          restExprAri <- typeArity restExprTy
          let preImplicits :: Arity -> [IsImplicit]
              preImplicits = takeWhile isImplicitOrInstance . map (^. arityParameterImplicit) . unfoldArity
              preImplicitsTypeRest = preImplicits restExprAri
              preAriHint = preImplicits ariHint
              preImplicitsInType =
                length
                  ( takeWhile
                      isImplicitOrInstance
                      (map fst defaults ++ preImplicitsTypeRest)
                  )
          loc <- getLoc <$> gets (^. appBuilderLeft)
          let numberOfExtraHoles = preImplicitsInType - length preAriHint
              toBeInserted :: [(IsImplicit, Maybe FunctionDefaultInfo)] =
                take numberOfExtraHoles (defaults <> map (,Nothing) preImplicitsTypeRest)
              mkHoleArg :: (IsImplicit, Maybe FunctionDefaultInfo) -> Sem r' AppBuilderArg
              mkHoleArg (i, mdef) = do
                (_appArg, _appBuilderArgIsDefault) <- case i of
                  Explicit -> impossible
                  ImplicitInstance -> (,ItIsNotDefault) <$> newHoleInstance loc
                  Implicit -> case mdef of
                    Nothing -> (,ItIsNotDefault) <$> newHoleImplicit loc
                    Just (FunctionDefaultInfo uid def) -> return (def, ItIsDefault uid)
                return
                  AppBuilderArg
                    { _appBuilderArg =
                        ApplicationArg
                          { _appArgIsImplicit = i,
                            _appArg
                          },
                      _appBuilderArgIsDefault
                    }
          trailingHoles <- mapM mkHoleArg toBeInserted
          mapM_ addTrailingHole trailingHoles
          where
            peelDefault :: BuilderType -> ([(IsImplicit, Maybe FunctionDefaultInfo)], Expression)
            peelDefault = run . runOutputList . go
              where
                go :: BuilderType -> Sem '[Output (IsImplicit, Maybe FunctionDefaultInfo)] Expression
                go = \case
                  BuilderTypeNoDefaults e -> return e
                  BuilderTypeDefaults d -> do
                    let impl = d ^. functionDefaultLeft . paramImplicit
                    output (impl, d ^. functionDefaultDefault)
                    go (d ^. functionDefaultRight)

            addTrailingHole :: AppBuilderArg -> Sem r' ()
            addTrailingHole holeArg0 = do
              holeArg <- applyCtx holeArg0
              fun <- peekFunctionType (holeArg ^. appBuilderArg . appArgIsImplicit)
              modify' (over appBuilderArgs (holeArg :))
              checkMatchingArg holeArg fun

        checkLoop :: AppBuilderArg -> Sem r' ()
        checkLoop arg = case arg ^. appBuilderArgIsDefault of
          ItIsNotDefault -> return ()
          ItIsDefault uid -> do
            st <- asks (^. insertedArgsStack)
            case span (/= uid) st of
              (_, []) -> return ()
              (c, _) ->
                let cyc = NonEmpty.reverse (uid :| c)
                 in throw (ErrDefaultArgLoop (DefaultArgLoop cyc))

        checkMatchingArg :: AppBuilderArg -> FunctionDefault -> Sem r' ()
        checkMatchingArg arg fun = do
          dropArg
          let funParam = fun ^. functionDefaultLeft
              funL = funParam ^. paramType
              checkLeft :: Sem r' Expression
              checkLeft = do
                checkLoop arg
                let adjustCtx = case fun ^. functionDefaultDefault of
                      Nothing -> id
                      Just dinfo -> local (over insertedArgsStack (dinfo ^. functionDefaultArgId :))
                adjustCtx (checkExpression funL (arg ^. appBuilderArg . appArg))
          arg' <- checkLeft
          let applyArg :: Sem r' ()
              applyArg = do
                extendCtx funParam arg'
                output
                  InsertedArg
                    { _insertedImplicit = arg ^. appBuilderArg . appArgIsImplicit,
                      _insertedValue = arg',
                      _insertedArgDefault = case arg ^. appBuilderArgIsDefault of
                        ItIsDefault {} -> True
                        ItIsNotDefault -> False
                    }
          modify' (set appBuilderType (fun ^. functionDefaultRight))
          applyArg

        goNextArg :: AppBuilderArg -> Sem r' ()
        goNextArg arg = do
          let i = arg ^. appBuilderArg . appArgIsImplicit
          fun <- peekFunctionType i
          insertMiddleHoleOrCheck fun i
          where
            insertMiddleHoleOrCheck :: FunctionDefault -> IsImplicit -> Sem r' ()
            insertMiddleHoleOrCheck fun argImpl =
              let funParam = fun ^. functionDefaultLeft
                  funImpl = funParam ^. paramImplicit
                  checkThisArg = checkMatchingArg arg fun >> goArgs
               in case (argImpl, funImpl) of
                    (Explicit, Explicit) -> checkThisArg
                    (Implicit, Implicit) -> checkThisArg
                    (ImplicitInstance, ImplicitInstance) -> checkThisArg
                    (ImplicitInstance, Explicit) -> throwExpectedExplicit (arg ^. appBuilderArg)
                    (Implicit, Explicit) -> throwExpectedExplicit (arg ^. appBuilderArg)
                    (Explicit, Implicit) -> insertMiddleHole Implicit
                    (ImplicitInstance, Implicit) -> insertMiddleHole Implicit
                    (Implicit, ImplicitInstance) -> insertMiddleHole ImplicitInstance
                    (Explicit, ImplicitInstance) -> insertMiddleHole ImplicitInstance
              where
                insertMiddleHole :: IsImplicit -> Sem r' ()
                insertMiddleHole impl = do
                  l <- gets (^. appBuilderLeft)
                  let loc = getLoc l
                  (h, _appBuilderArgIsDefault) <- case impl of
                    ImplicitInstance -> (,ItIsNotDefault) <$> newHoleInstance loc
                    Explicit -> impossible
                    Implicit -> case fun ^. functionDefaultDefault of
                      Nothing -> (,ItIsNotDefault) <$> newHoleImplicit loc
                      Just (FunctionDefaultInfo uid e) -> return (e, ItIsDefault uid)
                  let a =
                        AppBuilderArg
                          { _appBuilderArg = ApplicationArg impl h,
                            _appBuilderArgIsDefault
                          }
                  modify' (over appBuilderArgs (a :))
                  goArgs

        throwExpectedExplicit :: ApplicationArg -> Sem r' a
        throwExpectedExplicit arg = do
          throw
            . ErrArityCheckerError
            $ ErrExpectedExplicitArgument
              ExpectedExplicitArgument
                { _expectedExplicitArgument = arg
                }

        peekFunctionType :: IsImplicit -> Sem r' FunctionDefault
        peekFunctionType impl = do
          bty <- gets (^. appBuilderType)
          x <- case bty of
            BuilderTypeNoDefaults ty -> fromNoDefault <$> peekFunctionNoDefaults ty
            BuilderTypeDefaults s -> return s
          applyCtx x
          where
            fromNoDefault :: Function -> FunctionDefault
            fromNoDefault f =
              FunctionDefault
                { _functionDefaultLeft = f ^. functionLeft,
                  _functionDefaultDefault = Nothing,
                  _functionDefaultRight = BuilderTypeNoDefaults (f ^. functionRight)
                }
            peekFunctionNoDefaults :: Expression -> Sem r' Function
            peekFunctionNoDefaults ty0 = do
              ty <- weakNormalize ty0
              case ty of
                ExpressionFunction f -> return f
                ExpressionHole h -> holeRefineToFunction impl h
                _ -> throwExpectedFunTy
                  where
                    throwExpectedFunTy :: Sem r' a
                    throwExpectedFunTy = do
                      l <- gets (^. appBuilderLeft)
                      builderTy <- gets (^. appBuilderType)
                      args <- gets (^. appBuilderArgs)
                      let a :: Expression = foldApplication l (map (^. appBuilderArg) args)
                      throw $
                        ErrExpectedFunctionType
                          ExpectedFunctionType
                            { _expectedFunctionTypeExpression = a,
                              _expectedFunctionTypeLeft = l,
                              _expectedFunctionTypeType = (mkFinalBuilderType builderTy)
                            }

        dropArg :: Sem r' ()
        dropArg = modify' (over appBuilderArgs (drop 1))

        peekArg :: Sem r' (Maybe AppBuilderArg)
        peekArg = do
          b <- get
          return (head <$> nonEmpty (b ^. appBuilderArgs))

viewInductiveApp ::
  (Members '[Error TypeCheckerError, Inference, ResultBuilder] r) =>
  Expression ->
  Sem r (Either Hole (InductiveName, [Expression]))
viewInductiveApp ty = do
  ty' <- weakNormalize ty
  let (t, as) = viewTypeApp ty'
  case t of
    ExpressionIden (IdenInductive n) -> return (Right (n, as))
    ExpressionHole h -> do
      r <- queryMetavar h
      case r of
        Just h' -> viewInductiveApp h'
        Nothing -> return (Left h)
    _ -> throw (ErrInvalidPatternMatching (InvalidPatternMatching ty))
  where
    viewTypeApp :: Expression -> (Expression, [Expression])
    viewTypeApp tyapp = case tyapp of
      ExpressionApplication (Application l r _) ->
        second (`snoc` r) (viewTypeApp l)
      _ -> (tyapp, [])

typeArity :: forall r. (HasCallStack, Members '[Inference, Reader LocalVars] r) => Expression -> Sem r Arity
typeArity = weakNormalize >=> go
  where
    go :: Expression -> Sem r Arity
    go = \case
      ExpressionIden i -> goIden i
      ExpressionApplication a -> goApplication a
      ExpressionLiteral l -> goLiteral (l ^. withLocParam)
      ExpressionFunction f -> ArityFunction <$> goFun f
      ExpressionHole h -> return (ArityBlocking (BlockingHole h))
      ExpressionInstanceHole {} -> return ArityUnit
      ExpressionNatural {} -> return ArityUnit
      ExpressionLambda {} -> return ArityError
      ExpressionCase {} -> return ArityNotKnown -- TODO Do better here
      ExpressionUniverse {} -> return ArityUnit
      ExpressionSimpleLambda l -> ArityFunction <$> goSimpleLambda l
      ExpressionLet l -> goLet l

    -- It will be a type error since there are no literals that are types at the moment
    goLiteral :: Literal -> Sem r Arity
    goLiteral _ = return ArityError

    -- TODO could we do better here?
    goApplication :: Application -> Sem r Arity
    goApplication a = case lhs of
      ExpressionIden IdenInductive {} -> return ArityUnit
      _ -> return ArityNotKnown
      where
        lhs :: Expression
        lhs = fst (unfoldApplication a)

    -- TODO use the type info in the let clauses
    goLet :: Let -> Sem r Arity
    goLet l = typeArity (l ^. letExpression)

    goIden :: Iden -> Sem r Arity
    goIden = \case
      IdenVar v -> return (ArityBlocking (BlockingVar v))
      IdenInductive {} -> return ArityUnit
      IdenFunction {} -> return ArityNotKnown
      IdenConstructor {} -> return ArityError
      IdenAxiom {} -> return ArityNotKnown

    goParam :: FunctionParameter -> Sem r ArityParameter
    goParam FunctionParameter {..} = do
      paramAri' <- case _paramImplicit of
        Explicit -> go _paramType
        Implicit -> go _paramType
        ImplicitInstance -> return ArityUnit
      return
        ArityParameter
          { _arityParameterArity = paramAri',
            _arityParameterImplicit = _paramImplicit,
            _arityParameterInfo = emptyArgInfo
          }

    goFun :: Function -> Sem r FunctionArity
    goFun (Function l r) = do
      l' <- goParam l
      r' <- go r
      return
        FunctionArity
          { _functionArityLeft = l',
            _functionArityRight = r'
          }

    goSimpleLambda :: SimpleLambda -> Sem r FunctionArity
    goSimpleLambda s = goFun (simpleLambdaToFunction s)

guessArity ::
  forall r.
  (Members '[ResultBuilder, Reader InfoTable, Inference, Reader LocalVars] r) =>
  Expression ->
  Sem r Arity
guessArity = \case
  ExpressionHole {} -> return ArityNotKnown
  ExpressionInstanceHole {} -> return ArityUnit
  ExpressionFunction f -> return (arityFunction f)
  ExpressionNatural {} -> return ArityUnit
  ExpressionLiteral {} -> return arityLiteral
  ExpressionApplication a -> appHelper a
  ExpressionIden i -> idenHelper i
  ExpressionUniverse {} -> return arityUniverse
  ExpressionSimpleLambda l -> return (aritySimpleLambda l)
  ExpressionLambda l -> arityLambda l
  ExpressionLet l -> arityLet l
  ExpressionCase l -> arityCase l
  where
    idenHelper :: Iden -> Sem r Arity
    idenHelper = idenArity

    arityFunction :: Function -> Arity
    arityFunction = const ArityUnit

    aritySimpleLambda :: SimpleLambda -> Arity
    aritySimpleLambda s = arityFunction (simpleLambdaToFunction s)

    appHelper :: Application -> Sem r Arity
    appHelper a = do
      f' <- guessArity f
      let u = unfoldArity' f'
      return $ case refine args (u ^. ufoldArityParams) of
        Nothing -> ArityNotKnown
        Just a' -> foldArity (set ufoldArityParams a' u)
      where
        (f, args) = second (map (^. appArgIsImplicit) . toList) (unfoldApplication' a)

        refine :: [IsImplicit] -> [ArityParameter] -> Maybe [ArityParameter]
        refine as ps = case (as, ps) of
          (Explicit : as', ArityParameter {_arityParameterImplicit = Explicit} : ps') -> refine as' ps'
          (Implicit : as', ArityParameter {_arityParameterImplicit = Implicit} : ps') -> refine as' ps'
          (ImplicitInstance : as', ArityParameter {_arityParameterImplicit = ImplicitInstance} : ps') -> refine as' ps'
          (as'@(Explicit : _), ArityParameter {_arityParameterImplicit = Implicit} : ps') -> refine as' ps'
          (as'@(Explicit : _), ArityParameter {_arityParameterImplicit = ImplicitInstance} : ps') -> refine as' ps'
          (Implicit : _, ArityParameter {_arityParameterImplicit = Explicit} : _) -> Nothing
          (ImplicitInstance : _, ArityParameter {_arityParameterImplicit = Explicit} : _) -> Nothing
          (Implicit : _, ArityParameter {_arityParameterImplicit = ImplicitInstance} : _) -> Nothing
          (ImplicitInstance : _, ArityParameter {_arityParameterImplicit = Implicit} : _) -> Nothing
          ([], ps') -> Just ps'
          (_ : _, []) -> Nothing

arityLiteral :: Arity
arityLiteral = ArityUnit

arityUniverse :: Arity
arityUniverse = ArityUnit

arityLambda :: forall r. (Members '[ResultBuilder, Reader InfoTable, Inference, Reader LocalVars] r) => Lambda -> Sem r Arity
arityLambda l = do
  aris <- mapM guessClauseArity (l ^. lambdaClauses)
  return $
    if
        | allSame aris -> head aris
        | otherwise -> ArityNotKnown
  where
    guessClauseArity :: LambdaClause -> Sem r Arity
    guessClauseArity cl = do
      body <- unfoldArity' <$> guessArity (cl ^. lambdaBody)
      let ps = guessPatternArgArity <$> cl ^. lambdaPatterns
          uari' =
            UnfoldedArity
              { _ufoldArityParams = toList ps <> body ^. ufoldArityParams,
                _ufoldArityRest = body ^. ufoldArityRest
              }
      return (foldArity uari')

guessPatternArity :: Pattern -> Arity
guessPatternArity = \case
  PatternVariable {} -> ArityNotKnown
  PatternWildcardConstructor {} -> ArityUnit
  PatternConstructorApp {} -> ArityUnit

guessPatternArgArity :: PatternArg -> ArityParameter
guessPatternArgArity p =
  ArityParameter
    { _arityParameterArity = guessPatternArity (p ^. patternArgPattern),
      _arityParameterImplicit = p ^. patternArgIsImplicit,
      _arityParameterInfo =
        ArgInfo
          { _argInfoDefault = Nothing,
            _argInfoName = Nothing
          }
    }

arityLet :: (Members '[ResultBuilder, Reader InfoTable, Inference, Reader LocalVars] r) => Let -> Sem r Arity
arityLet l = guessArity (l ^. letExpression)

-- | If all arities are the same, we return that, otherwise we return
-- ArityNotKnown. Probably something better can be done.
arityBranches :: NonEmpty Arity -> Arity
arityBranches l
  | allSame l = head l
  | otherwise = ArityNotKnown

-- | All branches should have the same arity.
arityCase :: (Members '[ResultBuilder, Reader InfoTable, Inference, Reader LocalVars] r) => Case -> Sem r Arity
arityCase c = arityBranches <$> mapM (arityCaseRhs . (^. caseBranchRhs)) (c ^. caseBranches)

aritySideIf :: (Members '[ResultBuilder, Reader InfoTable, Inference, Reader LocalVars] r) => SideIfs -> Sem r Arity
aritySideIf SideIfs {..} = do
  let bodies :: NonEmpty Expression =
        snocNonEmptyMaybe
          ((^. sideIfBranchBody) <$> _sideIfBranches)
          _sideIfElse
  arityBranches <$> mapM guessArity bodies

arityCaseRhs :: (Members '[ResultBuilder, Reader InfoTable, Inference, Reader LocalVars] r) => CaseBranchRhs -> Sem r Arity
arityCaseRhs = \case
  CaseBranchRhsExpression e -> guessArity e
  CaseBranchRhsIf s -> aritySideIf s

idenArity :: forall r. (Members '[Inference, ResultBuilder, Reader LocalVars, Reader InfoTable] r) => Iden -> Sem r Arity
idenArity i = case i of
  IdenVar v -> getLocalArity v
  IdenInductive {} -> idenType' >>= typeArity
  IdenFunction f -> do
    ari <- idenType' >>= typeArity
    defaults <- (^. functionInfoArgsInfo) <$> lookupFunction f
    return (addArgsInfo f defaults ari)
  IdenConstructor {} -> do
    idenType' >>= typeArity
  IdenAxiom {} -> idenType' >>= typeArity
  where
    idenType' :: Sem r Expression
    idenType' = (^. typedType) <$> idenType i

addArgsInfo :: FunctionName -> [ArgInfo] -> Arity -> Arity
addArgsInfo fun topArgs ari = unfoldingArity helper ari
  where
    helper :: UnfoldedArity -> UnfoldedArity
    helper topAris = over ufoldArityParams (go topArgs) topAris
      where
        go :: [ArgInfo] -> [ArityParameter] -> [ArityParameter]
        go infos params = case infos of
          [] -> params
          info : infos' -> case params of
            [] -> err
            para : params' ->
              set arityParameterInfo info para : go infos' params'
          where
            err :: [ArityParameter]
            err =
              impossibleError $
                "There are more ArgInfo than ArityParameter for function "
                  <> ppTrace fun
                  <> "\n"
                  <> "[ArgInfo] = "
                  <> ppTrace infos
                  <> "\n[ArityParameter] = "
                  <> ppTrace (topAris ^. ufoldArityParams)
                  <> "\nArity = "
                  <> ppTrace ari

getLocalArity :: (Members '[Reader LocalVars, Inference] r) => VarName -> Sem r Arity
getLocalArity v = do
  mty <- asks (^. localTypes . at v)
  maybe (return ArityNotKnown) typeArity mty

newHoleImplicit :: (Member NameIdGen r) => Interval -> Sem r Expression
newHoleImplicit loc = ExpressionHole . mkHole loc <$> freshNameId

newHoleInstance :: (Member NameIdGen r) => Interval -> Sem r Expression
newHoleInstance loc = ExpressionInstanceHole . mkInstanceHole loc <$> freshNameId

withEmptyInsertedArgsStack :: Sem (Reader InsertedArgsStack ': r) a -> Sem r a
withEmptyInsertedArgsStack = runReader (mempty @InsertedArgsStack)
