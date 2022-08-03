module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Checker
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Checker,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
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
  Members '[Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable] r =>
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
  Members '[Reader EntryPoint, State NegativeTypeParameters, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader EntryPoint, State NegativeTypeParameters, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable] r =>
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
  Members '[Reader EntryPoint, Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable, State NegativeTypeParameters] r =>
  InductiveDef ->
  Sem r InductiveDef
checkInductiveDef (InductiveDef name built params constrs pos) = runInferenceDef $ do
  constrs' <- mapM goConstructor constrs
  ty <- inductiveType name
  modify (HashMap.insert name ty)
  let d = InductiveDef name built params constrs' pos
  checkPositivity d
  return d
  where
    paramLocals :: LocalVars
    paramLocals =
      LocalVars
        { _localTypes = HashMap.fromList [(p ^. inductiveParamName, smallUniverseE (getLoc p)) | p <- params],
          _localTyMap = mempty
        }
    goConstructor :: InductiveConstructorDef -> Sem (Inference ': r) InductiveConstructorDef
    goConstructor (InductiveConstructorDef n cty ret) = do
      expectedRetTy <- constructorReturnType n
      cty' <- runReader paramLocals $ do
        void (checkIsType (getLoc ret) ret)
        mapM (checkIsType (getLoc n)) cty
      whenJustM (matchTypes expectedRetTy ret) (const (errRet expectedRetTy))
      let c' = InductiveConstructorDef n cty' ret
      registerConstructor c'
      return c'
      where
        errRet :: Expression -> Sem (Inference ': r) a
        errRet expected =
          throw
            ( ErrWrongReturnType
                WrongReturnType
                  { _wrongReturnTypeConstructorName = n,
                    _wrongReturnTypeExpected = expected,
                    _wrongReturnTypeActual = ret
                  }
            )

checkFunctionDef ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State FunctionsTable] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  funDef <- readerState @FunctionsTable $ runInferenceDef $ do
    _funDefType' <- runReader emptyLocalVars (checkFunctionDefType _funDefType)
    registerIden _funDefName _funDefType'
    _funDefClauses' <- mapM (checkFunctionClause _funDefType') _funDefClauses
    return
      FunctionDef
        { _funDefClauses = _funDefClauses',
          _funDefType = _funDefType',
          ..
        }
  registerFunctionDef funDef
  return funDef

checkIsType ::
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Interval ->
  Expression ->
  Sem r Expression
checkIsType = checkExpression . smallUniverseE

checkFunctionDefType ::
  forall r.
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Sem r Expression
checkFunctionDefType ty = do
  traverseOf_ (leafExpressions . _ExpressionHole) go ty
  checkIsType loc ty
  where
    loc = getLoc ty
    go :: Hole -> Sem r ()
    go h = freshMetavar h

checkExpression ::
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  e' <- inferExpression' e
  let inferredType = e' ^. typedType
  whenJustM (matchTypes expectedTy inferredType) (throw . err)
  return (e' ^. typedExpression)
  where
    err matchErr =
      ErrWrongType
        ( WrongType
            { _wrongTypeThing = Left e,
              _wrongTypeActual = matchErr ^. matchErrorRight,
              _wrongTypeExpected = matchErr ^. matchErrorLeft
            }
        )

checkFunctionParameter ::
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  FunctionParameter ->
  Sem r FunctionParameter
checkFunctionParameter (FunctionParameter mv i e) = do
  e' <- checkIsType (getLoc e) e
  return (FunctionParameter mv i e')

-- checkInductiveDef ::
--   Members '[Reader InfoTable, Error TypeCheckerError, State NegativeTypeParameters, Reader EntryPoint] r =>
--   InductiveDef ->
--   Sem r ()
-- checkInductiveDef ty@InductiveDef {..} = do
--   checkPositivity ty
--   mapM_ (checkConstructorDef ty) _inductiveConstructors

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
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap (^. typedExpression) . inferExpression'

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Expression
lookupVar v = HashMap.lookupDefault err v <$> asks (^. localTypes)
  where
    err = error $ "internal error: could not find var " <> ppTrace v

checkFunctionClauseBody ::
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Inference] r =>
  LocalVars ->
  Expression ->
  Expression ->
  Sem r Expression
checkFunctionClauseBody locals expectedTy body =
  runReader locals (checkExpression expectedTy body)

checkFunctionClause ::
  forall r.
  Members '[Reader InfoTable, Reader FunctionsTable, Error TypeCheckerError, NameIdGen, Inference] r =>
  Expression ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause clauseType FunctionClause {..} = do
  (locals, bodyTy) <- helper _clausePatterns clauseType
  let bodyTy' = substitutionE (localsToSubsE locals) bodyTy
  _clauseBody' <- checkFunctionClauseBody locals bodyTy' _clauseBody
  return
    FunctionClause
      { _clauseBody = _clauseBody',
        ..
      }
  where
    helper :: [PatternArg] -> Expression -> Sem r (LocalVars, Expression)
    helper pats ty = runState emptyLocalVars (go pats ty)
    go :: [PatternArg] -> Expression -> Sem (State LocalVars ': r) Expression
    go pats bodyTy = case pats of
      [] -> return bodyTy
      (p : ps) -> case bodyTy of
        ExpressionHole h -> do
          s <- queryMetavar h
          case s of
            Just h' -> go pats h'
            Nothing -> do
              freshMetavar h
              l <- ExpressionHole <$> freshHole (getLoc h)
              r <- ExpressionHole <$> freshHole (getLoc h)
              let fun = ExpressionFunction (Function (unnamedParameter l) r)
              whenJustM (matchTypes (ExpressionHole h) fun) impossible
              go pats fun
        _ -> case unfoldFunType bodyTy of
          ([], _) -> error "too many patterns"
          (par : pars, ret) -> do
            checkPattern _clauseName par p
            go ps (foldFunType pars ret)

typeOfArg :: FunctionParameter -> Expression
typeOfArg = (^. paramType)

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
  Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars, Inference, NameIdGen] r =>
  FunctionName ->
  FunctionParameter ->
  PatternArg ->
  Sem r ()
checkPattern funName = go
  where
    go :: FunctionParameter -> PatternArg -> Sem r ()
    go argTy patArg = do
      matchIsImplicit (argTy ^. paramImplicit) patArg
      tyVarMap <- fmap (ExpressionIden . IdenVar) . (^. localTyMap) <$> get
      let ty = substitutionE tyVarMap (typeOfArg argTy)
          pat = patArg ^. patternArgPattern
      case pat of
        PatternWildcard {} -> return ()
        PatternVariable v -> do
          modify (addType v ty)
          registerIden v ty
          case argTy ^. paramName of
            Just v' -> do
              modify (over localTyMap (HashMap.insert v' v))
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
                            _wrongCtorTypeActual = constrIndName,
                            _wrongCtorTypeFunName = funName
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

-- | Returns {A : Expression} → A
literalType :: Members '[NameIdGen] r => LiteralLoc -> Sem r TypedExpression
literalType l = do
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
  Members '[Reader InfoTable, Reader FunctionsTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference] r =>
  Expression ->
  Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionLiteral l -> goLiteral l
  ExpressionFunction f -> goFunction f
  ExpressionHole h -> inferHole h
  ExpressionUniverse u -> goUniverse u
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

    goLambda :: Lambda -> Sem r TypedExpression
    goLambda (Lambda v ty b) = do
      b' <- inferExpression' b
      let smallUni = smallUniverseE (getLoc ty)
      ty' <- checkExpression smallUni ty
      let fun = Function (unnamedParameter smallUni) (b' ^. typedType)
      return
        TypedExpression
          { _typedType = ExpressionFunction fun,
            _typedExpression = ExpressionLambda (Lambda v ty' (b' ^. typedExpression))
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
            -- case l' ^. typedExpression of
            --   ExpressionLambda (Lambda lamVar _ lamBody) ->
            --     return
            --       TypedExpression
            --         { _typedExpression = substitutionE (HashMap.singleton lamVar r') lamBody,
            --           _typedType = substitutionApp (paraName, r') funR
            --         }
            --   _ ->
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
  Members '[Error TypeCheckerError, Inference] r =>
  Expression ->
  Sem r (Either Hole (InductiveName, [Expression]))
viewInductiveApp ty = case t of
  ExpressionIden (IdenInductive n) -> return (Right (n, as))
  ExpressionHole h -> do
    r <- queryMetavar h
    case r of
      Just h' -> viewInductiveApp h'
      Nothing -> return (Left h)
  _ -> throw (ErrImpracticalPatternMatching (ImpracticalPatternMatching ty))
  where
    (t, as) = viewTypeApp ty

viewTypeApp :: Expression -> (Expression, [Expression])
viewTypeApp t = case t of
  ExpressionApplication (Application l r _) ->
    second (`snoc` r) (viewTypeApp l)
  _ -> (t, [])
