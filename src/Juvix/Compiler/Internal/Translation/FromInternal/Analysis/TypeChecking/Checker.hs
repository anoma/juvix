module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Checker
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Checker,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Checker
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

registerConstructor :: (Members '[State TypesTable, Reader InfoTable] r) => InductiveConstructorDef -> Sem r ()
registerConstructor ctr = do
  ty <- constructorType (ctr ^. inductiveConstructorName)
  modify (HashMap.insert (ctr ^. inductiveConstructorName) ty)

checkModule ::
  (Members '[Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r) =>
  Module ->
  Sem r Module
checkModule Module {..} = do
  _moduleBody' <-
    (evalState (mempty :: NegativeTypeParameters) . checkModuleBody) _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  (Members '[Reader EntryPoint, State NegativeTypeParameters, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r) =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  (Members '[Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r) =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  (Members '[Reader EntryPoint, State NegativeTypeParameters, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r) =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction funs -> StatementFunction <$> runReader emptyLocalVars (checkMutualBlock funs)
  StatementForeign {} -> return s
  StatementInductive ind -> StatementInductive <$> readerState @FunctionsTable (checkInductiveDef ind)
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementAxiom ax -> do
    modify (HashMap.insert (ax ^. axiomName) (ax ^. axiomType))
    return s

checkInductiveDef ::
  forall r.
  (Members '[Reader EntryPoint, Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, State NegativeTypeParameters, Output Example, Builtins] r) =>
  InductiveDef ->
  Sem r InductiveDef
checkInductiveDef InductiveDef {..} = runInferenceDef $ do
  constrs' <- mapM goConstructor _inductiveConstructors
  ty <- inductiveType _inductiveName
  modify (HashMap.insert _inductiveName ty)
  examples' <- mapM checkExample _inductiveExamples
  let d =
        InductiveDef
          { _inductiveConstructors = constrs',
            _inductiveExamples = examples',
            _inductiveName,
            _inductiveBuiltin,
            _inductivePositive,
            _inductiveParameters
          }
  checkPositivity d
  return d
  where
    paramLocals :: LocalVars
    paramLocals =
      LocalVars
        { _localTypes = HashMap.fromList [(p ^. inductiveParamName, smallUniverseE (getLoc p)) | p <- _inductiveParameters],
          _localTyMap = mempty
        }
    goConstructor :: InductiveConstructorDef -> Sem (Inference ': r) InductiveConstructorDef
    goConstructor (InductiveConstructorDef {..}) = do
      expectedRetTy <- constructorReturnType _inductiveConstructorName
      cty' <- runReader paramLocals $ do
        void (checkIsType (getLoc ret) ret)
        mapM (checkIsType (getLoc _inductiveConstructorName)) _inductiveConstructorParameters
      examples' <- mapM checkExample _inductiveConstructorExamples
      whenJustM (matchTypes expectedRetTy ret) (const (errRet expectedRetTy))
      let c' =
            InductiveConstructorDef
              { _inductiveConstructorParameters = cty',
                _inductiveConstructorExamples = examples',
                _inductiveConstructorReturnType,
                _inductiveConstructorName
              }
      registerConstructor c'
      return c'
      where
        ret = _inductiveConstructorReturnType
        errRet :: Expression -> Sem (Inference ': r) a
        errRet expected =
          throw
            ( ErrWrongReturnType
                WrongReturnType
                  { _wrongReturnTypeConstructorName = _inductiveConstructorName,
                    _wrongReturnTypeExpected = expected,
                    _wrongReturnTypeActual = ret
                  }
            )

withEmptyVars :: Sem (Reader LocalVars : r) a -> Sem r a
withEmptyVars = runReader emptyLocalVars

checkMutualBlock ::
  (Members '[Reader LocalVars, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r) =>
  MutualBlock ->
  Sem r MutualBlock
checkMutualBlock (MutualBlock ds) =
  readerState @FunctionsTable $
    MutualBlock <$> runInferenceDefs (mapM checkFunctionDef ds)

checkFunctionDef ::
  (Members '[Reader LocalVars, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins, Inference] r) =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  funDef <- readerState @FunctionsTable $ do
    _funDefType' <- checkFunctionDefType _funDefType
    registerIden _funDefName _funDefType'
    _funDefClauses' <- mapM (checkFunctionClause _funDefType') _funDefClauses
    return
      FunctionDef
        { _funDefClauses = _funDefClauses',
          _funDefType = _funDefType',
          ..
        }
  registerFunctionDef funDef
  readerState @FunctionsTable (traverseOf funDefExamples (mapM checkExample) funDef)

checkIsType ::
  (Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable] r) =>
  Interval ->
  Expression ->
  Sem r Expression
checkIsType = checkExpression . smallUniverseE

checkFunctionDefType ::
  forall r.
  (Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable] r) =>
  Expression ->
  Sem r Expression
checkFunctionDefType ty = checkIsType loc ty
  where
    loc = getLoc ty

checkExample ::
  (Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Output Example, State TypesTable] r) =>
  Example ->
  Sem r Example
checkExample e = do
  e' <- withEmptyVars (runInferenceDef (traverseOf exampleExpression (inferExpression Nothing >=> strongNormalize) e))
  output e'
  return e'

checkExpression ::
  forall r.
  (Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Reader LocalVars, Inference, Output Example, State TypesTable] r) =>
  Expression ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  e' <- inferExpression' (Just expectedTy) e
  let inferredType = e' ^. typedType
  whenJustM (matchTypes expectedTy inferredType) (const (err inferredType))
  return (e' ^. typedExpression)
  where
    err :: Expression -> Sem r a
    err inferred = do
      inferred' <- strongNormalize inferred
      expected' <- strongNormalize expectedTy
      throw $
        ErrWrongType
          ( WrongType
              { _wrongTypeThing = Left e,
                _wrongTypeActual = inferred',
                _wrongTypeExpected = expected'
              }
          )

checkFunctionParameter ::
  (Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable] r) =>
  FunctionParameter ->
  Sem r FunctionParameter
checkFunctionParameter (FunctionParameter mv i e) = do
  e' <- checkIsType (getLoc e) e
  return (FunctionParameter mv i e')

checkConstructorDef ::
  ( Members
      '[ Reader EntryPoint,
         Reader InfoTable,
         Error TypeCheckerError,
         State NegativeTypeParameters
       ]
      r
  ) =>
  InductiveDef ->
  InductiveConstructorDef ->
  Sem r ()
checkConstructorDef ty ctor = do
  checkConstructorReturnType ty ctor

checkConstructorReturnType ::
  (Members '[Reader InfoTable, Error TypeCheckerError] r) =>
  InductiveDef ->
  InductiveConstructorDef ->
  Sem r ()
checkConstructorReturnType indType ctor = do
  let ctorName = ctor ^. inductiveConstructorName
      ctorReturnType = ctor ^. inductiveConstructorReturnType
      tyName = indType ^. inductiveName
      indParams = map (^. inductiveParamName) (indType ^. inductiveParameters)
      expectedReturnType =
        foldExplicitApplication
          (ExpressionIden (IdenInductive tyName))
          (map (ExpressionIden . IdenVar) indParams)
  when
    (ctorReturnType /= expectedReturnType)
    ( throw
        ( ErrWrongReturnType
            ( WrongReturnType
                { _wrongReturnTypeConstructorName = ctorName,
                  _wrongReturnTypeExpected = expectedReturnType,
                  _wrongReturnTypeActual = ctorReturnType
                }
            )
        )
    )

inferExpression ::
  (Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins, Output Example, State TypesTable] r) =>
  Maybe Expression -> -- type hint
  Expression ->
  Sem r Expression
inferExpression hint = fmap (^. typedExpression) . inferExpression' hint

lookupVar :: (Member (Reader LocalVars) r) => Name -> Sem r Expression
lookupVar v = HashMap.lookupDefault err v <$> asks (^. localTypes)
  where
    err = error $ "internal error: could not find var " <> ppTrace v

checkFunctionClause ::
  forall r.
  (Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Inference, Builtins, State TypesTable, Output Example, Reader LocalVars] r) =>
  Expression ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause clauseType FunctionClause {..} = do
  body' <- checkClause clauseType _clausePatterns _clauseBody
  return
    FunctionClause
      { _clauseBody = body',
        ..
      }

-- | helper function for function clauses and lambda functions
checkClause ::
  forall r.
  (Members '[Reader InfoTable, Reader FunctionsTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Builtins, Output Example, State TypesTable] r) =>
  -- | Type
  Expression ->
  -- | Arguments
  [PatternArg] ->
  -- | Body
  Expression ->
  Sem r Expression -- Checked body
checkClause clauseType clausePats body = do
  locals0 <- ask
  (localsPats, bodyTy) <- helper clausePats clauseType
  let locals' = locals0 <> localsPats
      bodyTy' = substitutionE (localsToSubsE locals') bodyTy
  local (const locals') (checkExpression bodyTy' body)
  where
    helper :: [PatternArg] -> Expression -> Sem r (LocalVars, Expression)
    helper pats ty = runState emptyLocalVars (go pats ty)
    go :: [PatternArg] -> Expression -> Sem (State LocalVars ': r) Expression
    go pats bodyTy = case pats of
      [] -> return bodyTy
      (p : ps) -> do
        bodyTy' <- weakNormalize bodyTy
        case bodyTy' of
          ExpressionHole h -> do
            fun <- holeRefineToFunction h
            go pats (ExpressionFunction fun)
          _ -> case unfoldFunType bodyTy' of
            ([], _) -> error "too many patterns"
            (par : pars, ret) -> do
              checkPattern par p
              go ps (foldFunType pars ret)

-- | Refines a hole into a function type. I.e. '_@1' is matched with '_@fresh → _@fresh'
holeRefineToFunction :: (Members '[Inference, NameIdGen] r) => Hole -> Sem r Function
holeRefineToFunction h = do
  s <- queryMetavar h
  case s of
    Just h' -> case h' of
      ExpressionFunction f -> return f
      ExpressionHole h'' -> holeRefineToFunction h''
      _ -> error "cannot refine hole to function"
    Nothing -> do
      l <- ExpressionHole <$> freshHole (getLoc h)
      r <- ExpressionHole <$> freshHole (getLoc h)
      let fun = Function (unnamedParameter l) r
      whenJustM (matchTypes (ExpressionHole h) (ExpressionFunction fun)) impossible
      return fun

matchIsImplicit :: (Member (Error TypeCheckerError) r) => IsImplicit -> PatternArg -> Sem r ()
matchIsImplicit expected actual =
  unless
    (expected == actual ^. patternArgIsImplicit)
    ( throw
        ( ErrArity
            ( ErrWrongPatternIsImplicit
                WrongPatternIsImplicit
                  { _wrongPatternIsImplicitExpected = expected,
                    _wrongPatternIsImplicitActual = actual
                  }
            )
        )
    )

checkPattern ::
  forall r.
  (Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars, Inference, NameIdGen, Reader FunctionsTable] r) =>
  FunctionParameter ->
  PatternArg ->
  Sem r ()
checkPattern = go
  where
    go :: FunctionParameter -> PatternArg -> Sem r ()
    go argTy patArg = do
      matchIsImplicit (argTy ^. paramImplicit) patArg
      tyVarMap <- fmap (ExpressionIden . IdenVar) . (^. localTyMap) <$> get
      let ty = substitutionE tyVarMap (argTy ^. paramType)
          pat = patArg ^. patternArgPattern
          name = patArg ^. patternArgName
      whenJust name (\n -> addVar n ty argTy)
      case pat of
        PatternVariable v -> addVar v ty argTy
        PatternConstructorApp a -> do
          s <- checkSaturatedInductive ty
          info <- lookupConstructor (a ^. constrAppConstructor)
          let constrIndName = info ^. constructorInfoInductive
              constrName = a ^. constrAppConstructor
              err :: MatchError -> Sem r ()
              err m =
                throw
                  ( ErrWrongType
                      WrongType
                        { _wrongTypeThing = Right pat,
                          _wrongTypeExpected = m ^. matchErrorRight,
                          _wrongTypeActual = m ^. matchErrorLeft
                        }
                  )
          case s of
            Left hole -> do
              let indParams = info ^. constructorInfoInductiveParameters
                  numIndParams = length indParams
                  indName :: Iden
                  indName = IdenInductive (info ^. constructorInfoInductive)
                  loc = getLoc a
              paramHoles <- map ExpressionHole <$> replicateM numIndParams (freshHole loc)
              let patternTy = foldApplication (ExpressionIden indName) (zip (repeat Explicit) paramHoles)
              whenJustM
                (matchTypes patternTy (ExpressionHole hole))
                err
              let tyArgs = zipExact indParams paramHoles
              goConstr a tyArgs
            Right (ind, tyArgs) -> do
              when
                (ind /= constrIndName)
                ( throw
                    ( ErrWrongConstructorType
                        WrongConstructorType
                          { _wrongCtorTypeName = constrName,
                            _wrongCtorTypeExpected = ind,
                            _wrongCtorTypeActual = constrIndName
                          }
                    )
                )
              goConstr a tyArgs
      where
        addVar :: VarName -> Expression -> FunctionParameter -> Sem r ()
        addVar v ty argType = do
          modify (addType v ty)
          registerIden v ty
          whenJust (argType ^. paramName) (\v' -> modify (addTypeMapping v' v))
        goConstr :: ConstructorApp -> [(InductiveParameter, Expression)] -> Sem r ()
        goConstr app@(ConstructorApp c ps) ctx = do
          (_, psTys) <- constructorArgTypes <$> lookupConstructor c
          let psTys' = map (substituteIndParams ctx) psTys
              expectedNum = length psTys
          let w = map unnamedParameter psTys'
          when (expectedNum /= length ps) (throw (appErr app expectedNum))
          zipWithM_ go w ps
        appErr :: ConstructorApp -> Int -> TypeCheckerError
        appErr app expected =
          ErrArity
            ( ErrWrongConstructorAppLength
                ( WrongConstructorAppLength
                    { _wrongConstructorAppLength = app,
                      _wrongConstructorAppLengthExpected = expected
                    }
                )
            )

    checkSaturatedInductive :: Expression -> Sem r (Either Hole (InductiveName, [(InductiveParameter, Expression)]))
    checkSaturatedInductive ty = do
      i <- viewInductiveApp ty
      case i of
        Left hole -> return (Left hole)
        Right (ind, args) -> do
          params :: [InductiveParameter] <-
            (^. inductiveInfoDef . inductiveParameters)
              <$> lookupInductive ind
          let numArgs = length args
              numParams = length params
          when
            (numArgs < numParams)
            ( throw
                ( ErrTooFewArgumentsIndType
                    ( WrongNumberArgumentsIndType
                        { _wrongNumberArgumentsIndTypeActualType = ty,
                          _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                          _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                        }
                    )
                )
            )
          when
            (numArgs > numParams)
            ( throw
                ( ErrTooManyArgumentsIndType
                    ( WrongNumberArgumentsIndType
                        { _wrongNumberArgumentsIndTypeActualType = ty,
                          _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                          _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                        }
                    )
                )
            )
          return (Right (ind, zipExact params args))

freshHole :: (Members '[Inference, NameIdGen] r) => Interval -> Sem r Hole
freshHole l = do
  uid <- freshNameId
  return (Hole uid l)

literalType :: (Members '[NameIdGen, Builtins] r) => LiteralLoc -> Sem r TypedExpression
literalType lit@(WithLoc i l) = case l of
  LitInteger {} -> do
    nat <- getBuiltinName i BuiltinNat
    return
      TypedExpression
        { _typedExpression = ExpressionLiteral lit,
          _typedType = ExpressionIden (IdenInductive nat)
        }
  LitString {} -> do
    str <- getBuiltinName i BuiltinString
    return
      TypedExpression
        { _typedExpression = ExpressionLiteral lit,
          _typedType = ExpressionIden (IdenAxiom str)
        }

inferExpression' ::
  forall r.
  (Members '[Reader InfoTable, Reader FunctionsTable, State TypesTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Output Example, Builtins] r) =>
  Maybe Expression ->
  Expression ->
  Sem r TypedExpression
inferExpression' hint e = case e of
  ExpressionIden i -> goIden i
  ExpressionApplication a -> goApplication a
  ExpressionLiteral l -> goLiteral l
  ExpressionFunction f -> goFunction f
  ExpressionHole h -> goHole h
  ExpressionUniverse u -> goUniverse u
  ExpressionSimpleLambda l -> goSimpleLambda l
  ExpressionLambda l -> goLambda l
  ExpressionLet l -> goLet l
  ExpressionCase l -> goCase l
  where
    goLet :: Let -> Sem r TypedExpression
    goLet l = do
      tbl <- ask
      (tbl', _letClauses) <- runState tbl (mapM goLetClause (l ^. letClauses))
      typedBody <- local (const tbl') (inferExpression' hint (l ^. letExpression))
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

    -- what about mutually recursive lets?
    goLetClause :: LetClause -> Sem (State FunctionsTable ': r) LetClause
    goLetClause = \case
      LetFunDef f -> LetFunDef <$> checkFunctionDef f

    goHole :: Hole -> Sem r TypedExpression
    goHole h = do
      void (queryMetavar h)
      return
        TypedExpression
          { _typedExpression = ExpressionHole h,
            _typedType = ExpressionUniverse (SmallUniverse (getLoc h))
          }

    goSimpleLambda :: SimpleLambda -> Sem r TypedExpression
    goSimpleLambda (SimpleLambda v ty b) = do
      b' <- inferExpression' Nothing b
      let smallUni = smallUniverseE (getLoc ty)
      ty' <- checkExpression smallUni ty
      let fun = Function (unnamedParameter smallUni) (b' ^. typedType)
      return
        TypedExpression
          { _typedType = ExpressionFunction fun,
            _typedExpression = ExpressionSimpleLambda (SimpleLambda v ty' (b' ^. typedExpression))
          }

    goCase :: Case -> Sem r TypedExpression
    goCase c = do
      ty <- case hint of
        Nothing -> ExpressionHole <$> freshHole (getLoc c)
        Just hi -> return hi
      typedCaseExpression <- inferExpression' Nothing (c ^. caseExpression)
      let _caseExpression = typedCaseExpression ^. typedExpression
          goBranch :: CaseBranch -> Sem r CaseBranch
          goBranch b =
            traverseOf
              caseBranchExpression
              (checkClause funty (pure (b ^. caseBranchPattern)))
              b
            where
              funty :: Expression
              funty = ExpressionFunction (mkFunction (typedCaseExpression ^. typedType) ty)
      _caseBranches <- mapM goBranch (c ^. caseBranches)
      let _caseParens = c ^. caseParens
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionCase Case {..}
          }

    goLambda :: Lambda -> Sem r TypedExpression
    goLambda l@(Lambda cl) = do
      ty <- case hint of
        Just hi -> return hi
        Nothing -> ExpressionHole <$> freshHole (getLoc l)
      l' <- Lambda <$> mapM (goClause ty) cl
      return
        TypedExpression
          { _typedType = ty,
            _typedExpression = ExpressionLambda l'
          }
      where
        goClause :: Expression -> LambdaClause -> Sem r LambdaClause
        goClause ty (LambdaClause pats body) = do
          body' <- checkClause ty (toList pats) body
          return (LambdaClause pats body')

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
          bodyEnv = case l ^. paramName of
            Nothing -> id
            Just v -> local (over localTypes (HashMap.insert v (l ^. paramType)))
      r' <- bodyEnv (checkExpression uni r)
      return (TypedExpression uni (ExpressionFunction (Function l' r')))

    goLiteral :: LiteralLoc -> Sem r TypedExpression
    goLiteral = literalType

    goIden :: Iden -> Sem r TypedExpression
    goIden i = case i of
      IdenFunction fun -> do
        info <- lookupFunction fun
        return (TypedExpression (info ^. functionInfoDef . funDefType) (ExpressionIden i))
      IdenConstructor c -> do
        ty <- constructorType c
        return (TypedExpression ty (ExpressionIden i))
      IdenVar v -> do
        ty <- lookupVar v
        return (TypedExpression ty (ExpressionIden i))
      IdenAxiom v -> do
        info <- lookupAxiom v
        return (TypedExpression (info ^. axiomInfoType) (ExpressionIden i))
      IdenInductive v -> do
        kind <- inductiveType v
        return (TypedExpression kind (ExpressionIden i))

    goApplication :: Application -> Sem r TypedExpression
    goApplication (Application l r iapp) = inferExpression' Nothing l >>= helper
      where
        helper :: TypedExpression -> Sem r TypedExpression
        helper l' = do
          l'ty <- weakNormalize (l' ^. typedType)
          case l'ty of
            ExpressionFunction (Function (FunctionParameter paraName ifun funL) funR) -> do
              r' <- checkExpression funL r
              unless (iapp == ifun) (error "implicitness mismatch")
              return
                TypedExpression
                  { _typedExpression =
                      ExpressionApplication
                        Application
                          { _appLeft = l' ^. typedExpression,
                            _appRight = r',
                            _appImplicit = iapp
                          },
                    _typedType = substitutionApp (paraName, r') funR
                  }
            ExpressionHole h -> do
              fun <- ExpressionFunction <$> holeRefineToFunction h
              helper (set typedType fun l')
            _ -> throw tyErr
              where
                tyErr :: TypeCheckerError
                tyErr =
                  ErrExpectedFunctionType
                    ( ExpectedFunctionType
                        { _expectedFunctionTypeExpression = e,
                          _expectedFunctionTypeApp = l,
                          _expectedFunctionTypeType = l' ^. typedType
                        }
                    )

viewInductiveApp ::
  (Members '[Error TypeCheckerError, Inference, Reader FunctionsTable] r) =>
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
    _ -> throw (ErrImpracticalPatternMatching (ImpracticalPatternMatching ty))
  where
    viewTypeApp :: Expression -> (Expression, [Expression])
    viewTypeApp tyapp = case tyapp of
      ExpressionApplication (Application l r _) ->
        second (`snoc` r) (viewTypeApp l)
      _ -> (tyapp, [])
