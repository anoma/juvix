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

registerConstructor :: Members '[State TypesTable, Reader InfoTable] r => InductiveConstructorDef -> Sem r ()
registerConstructor ctr = do
  ty <- constructorType (ctr ^. inductiveConstructorName)
  modify (HashMap.insert (ctr ^. inductiveConstructorName) ty)

checkModule ::
  Members '[Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r =>
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
  Members '[Reader EntryPoint, State NegativeTypeParameters, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader EntryPoint, State NegativeTypeParameters, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive ind -> StatementInductive <$> readerState @FunctionsTable (checkInductiveDef ind)
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementAxiom ax -> do
    modify (HashMap.insert (ax ^. axiomName) (ax ^. axiomType))
    return s

checkInductiveDef ::
  forall r.
  Members '[Reader EntryPoint, Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, State NegativeTypeParameters, Output Example, Builtins] r =>
  InductiveDef ->
  Sem r InductiveDef
-- checkInductiveDef (InductiveDef name built params constrs pos) = runInferenceDef $ do
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

checkFunctionDef ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, Output Example, Builtins] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  funDef <- readerState @FunctionsTable $ runInferenceDef $ do
    _funDefType' <- withEmptyVars (checkFunctionDefType _funDefType)
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
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins] r =>
  Interval ->
  Expression ->
  Sem r Expression
checkIsType = checkExpression . smallUniverseE

checkFunctionDefType ::
  forall r.
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins] r =>
  Expression ->
  Sem r Expression
checkFunctionDefType ty = checkIsType loc ty
  where
    loc = getLoc ty

checkExample ::
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Output Example, State TypesTable] r =>
  Example ->
  Sem r Example
checkExample e = do
  e' <- withEmptyVars (runInferenceDef (traverseOf exampleExpression (inferExpression >=> strongNormalize) e))
  output e'
  return e'

checkExpression :: forall r.
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, Builtins, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  e' <- inferExpression' e
  let inferredType = e' ^. typedType
  whenJustM (matchTypes expectedTy inferredType) (const (err inferredType))
  return (e' ^. typedExpression)
  where
    err :: Expression -> Sem r a
    err inferred = do
      inferred' <- strongNormalize inferred
      expected' <- strongNormalize expectedTy
      throw $ ErrWrongType
        ( WrongType
            { _wrongTypeThing = Left e,
              _wrongTypeActual = inferred',
              _wrongTypeExpected = expected'
            }
        )

checkFunctionParameter ::
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference, Builtins] r =>
  FunctionParameter ->
  Sem r FunctionParameter
checkFunctionParameter (FunctionParameter mv i e) = do
  e' <- checkIsType (getLoc e) e
  return (FunctionParameter mv i e')

checkConstructorDef ::
  Members
    '[ Reader EntryPoint,
       Reader InfoTable,
       Error TypeCheckerError,
       State NegativeTypeParameters
     ]
    r =>
  InductiveDef ->
  InductiveConstructorDef ->
  Sem r ()
checkConstructorDef ty ctor = do
  checkConstructorReturnType ty ctor

checkConstructorReturnType ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
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
  Members '[Reader InfoTable, Reader FunctionsTable, Builtins, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap (^. typedExpression) . inferExpression'

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Expression
lookupVar v = HashMap.lookupDefault err v <$> asks (^. localTypes)
  where
    err = error $ "internal error: could not find var " <> ppTrace v

-- checkClauseBody ::
--   Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Builtins, Inference] r =>
--   LocalVars ->
--   Expression ->
--   Expression ->
--   Sem r Expression
-- checkClauseBody locals expectedTy body =
--   runReader locals (checkExpression expectedTy body)

checkFunctionClause ::
  forall r.
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Builtins, Inference] r =>
  Expression ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause clauseType FunctionClause {..} = do
  body' <- runReader emptyLocalVars (checkClause clauseType _clausePatterns _clauseBody)
  return
    FunctionClause
      { _clauseBody = body',
        ..
      }

-- | helper function for function clauses and lambda functions
checkClause ::
  forall r.
  Members '[Reader InfoTable, Reader FunctionsTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Builtins, Inference] r =>
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
      (p : ps) -> case bodyTy of
        ExpressionHole h -> do
          fun <- holeRefineToFunction h
          go pats (ExpressionFunction fun)
        _ -> case unfoldFunType bodyTy of
          ([], _) -> error "too many patterns"
          (par : pars, ret) -> do
            checkPattern par p
            go ps (foldFunType pars ret)

-- | Refines a hole into a function type. I.e. '_@1' is matched with '_@fresh → _@fresh'
holeRefineToFunction :: Members '[Inference, NameIdGen] r => Hole -> Sem r Function
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

matchIsImplicit :: Member (Error TypeCheckerError) r => IsImplicit -> PatternArg -> Sem r ()
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
  Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars, Inference, NameIdGen, Reader FunctionsTable] r =>
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
      case pat of
        PatternWildcard {} -> return ()
        PatternVariable v -> do
          modify (addType v ty)
          registerIden v ty
          case argTy ^. paramName of
            Just v' -> modify (over localTyMap (HashMap.insert v' v))
            _ -> return ()
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

freshHole :: Members '[Inference, NameIdGen] r => Interval -> Sem r Hole
freshHole l = do
  uid <- freshNameId
  let h = Hole uid l
  freshMetavar h
  return h

literalType :: Members '[NameIdGen, Builtins] r => LiteralLoc -> Sem r TypedExpression
literalType lit@(WithLoc i l) = case l of
  LitInteger {} -> do
    nat <- getBuiltinName i BuiltinNatural
    return
      TypedExpression
        { _typedExpression = ExpressionLiteral lit,
          _typedType = ExpressionIden (IdenInductive nat)
        }
  LitString {} -> literalMagicType lit

-- | Returns {A : Expression} → A
literalMagicType :: Members '[NameIdGen] r => LiteralLoc -> Sem r TypedExpression
literalMagicType l = do
  uid <- freshNameId
  let strA :: Text
      strA = "A"
      typeVar =
        Name
          { _nameText = strA,
            _nameId = uid,
            _namePretty = strA,
            _nameKind = KNameLocal,
            _nameLoc = getLoc l
          }
      param =
        FunctionParameter
          { _paramName = Just typeVar,
            _paramImplicit = Implicit,
            _paramType = smallUniverseE (getLoc l)
          }
      type_ =
        ExpressionFunction
          Function
            { _functionLeft = param,
              _functionRight = ExpressionIden (IdenVar typeVar)
            }
  return
    TypedExpression
      { _typedType = type_,
        _typedExpression = ExpressionLiteral l
      }

inferExpression' ::
  forall r.
  Members '[Reader InfoTable, Reader FunctionsTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference, Builtins] r =>
  Expression ->
  Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionLiteral l -> goLiteral l
  ExpressionFunction f -> goFunction f
  ExpressionHole h -> inferHole h
  ExpressionUniverse u -> goUniverse u
  ExpressionSimpleLambda l -> goSimpleLambda l
  ExpressionLambda l -> goLambda l
  where
    inferHole :: Hole -> Sem r TypedExpression
    inferHole h = do
      freshMetavar h
      return
        TypedExpression
          { _typedExpression = ExpressionHole h,
            _typedType = ExpressionUniverse (SmallUniverse (getLoc h))
          }

    goSimpleLambda :: SimpleLambda -> Sem r TypedExpression
    goSimpleLambda (SimpleLambda v ty b) = do
      b' <- inferExpression' b
      let smallUni = smallUniverseE (getLoc ty)
      ty' <- checkExpression smallUni ty
      let fun = Function (unnamedParameter smallUni) (b' ^. typedType)
      return
        TypedExpression
          { _typedType = ExpressionFunction fun,
            _typedExpression = ExpressionSimpleLambda (SimpleLambda v ty' (b' ^. typedExpression))
          }

    goLambda :: Lambda -> Sem r TypedExpression
    goLambda l@(Lambda cl) = do
      h <- freshHole (getLoc l)
      l' <- Lambda <$> mapM (goClause h) cl
      return
        TypedExpression
          { _typedType = ExpressionHole h,
            _typedExpression = ExpressionLambda l'
          }
      where
        goClause :: Hole -> LambdaClause -> Sem r LambdaClause
        goClause h (LambdaClause pats body) = do
          let patArgs = map (PatternArg Explicit) (toList pats)
          body' <- checkClause (ExpressionHole h) patArgs body
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

    inferIden :: Iden -> Sem r TypedExpression
    inferIden i = case i of
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

    inferApplication :: Application -> Sem r TypedExpression
    inferApplication (Application l r iapp) = inferExpression' l >>= helper
      where
        helper :: TypedExpression -> Sem r TypedExpression
        helper l' = case l' ^. typedType of
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
          -- When we have have an application with a hole on the left: '_@1 x'
          -- We assume that it is a type application and thus 'x' must be a type.
          -- Not sure if this is always desirable.
          ExpressionHole h -> do
            q <- queryMetavar h
            case q of
              Just ty -> helper (set typedType ty l')
              Nothing -> do
                r' <- checkExpression (smallUniverseE (getLoc h)) r
                h' <- freshHole (getLoc h)
                let fun = Function (unnamedParameter r') (ExpressionHole h')
                whenJustM (matchTypes (ExpressionHole h) (ExpressionFunction fun)) impossible
                return
                  TypedExpression
                    { _typedType = ExpressionHole h',
                      _typedExpression =
                        ExpressionApplication
                          Application
                            { _appLeft = l' ^. typedExpression,
                              _appRight = r',
                              _appImplicit = iapp
                            }
                    }
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
  Members '[Error TypeCheckerError, Inference, Reader FunctionsTable] r =>
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
