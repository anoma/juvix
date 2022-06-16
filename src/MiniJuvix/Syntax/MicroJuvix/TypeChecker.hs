module MiniJuvix.Syntax.MicroJuvix.TypeChecker
  ( module MiniJuvix.Syntax.MicroJuvix.TypeChecker,
    module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
    module MiniJuvix.Syntax.MicroJuvix.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Internal.NameIdGen
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.MicroJuvix.Error
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.LocalVars
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import MiniJuvix.Syntax.MicroJuvix.TypeChecker.Inference

entryMicroJuvixTyped ::
  Members '[Error TypeCheckerError, NameIdGen] r =>
  MicroJuvixArityResult ->
  Sem r MicroJuvixTypedResult
entryMicroJuvixTyped res@MicroJuvixArityResult {..} = do
  r <- runReader table (mapM checkModule _resultModules)
  return
    MicroJuvixTypedResult
      { _resultMicroJuvixArityResult = res,
        _resultModules = r
      }
  where
    table :: InfoTable
    table = buildTable _resultModules

checkModule ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  Module ->
  Sem r Module
checkModule Module {..} = do
  _moduleBody' <- checkModuleBody _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = runInferenceDef $ do
  info <- lookupFunction _funDefName
  checkFunctionDefType _funDefType
  _funDefClauses' <- mapM (checkFunctionClause info) _funDefClauses
  return
    FunctionDef
      { _funDefClauses = _funDefClauses',
        ..
      }

checkFunctionDefType :: forall r. Members '[Inference] r => Type -> Sem r ()
checkFunctionDefType = go
  where
    go :: Type -> Sem r ()
    go t = case t of
      TypeHole h -> void (freshMetavar h)
      TypeIden {} -> return ()
      TypeApp a -> goApp a
      TypeFunction f -> goFunction f
      TypeAbs f -> goAbs f
      TypeUniverse -> return ()
    goApp :: TypeApplication -> Sem r ()
    goApp (TypeApplication a b _) = go a >> go b
    goFunction :: Function -> Sem r ()
    goFunction (Function a b) = go a >> go b
    goAbs :: TypeAbstraction -> Sem r ()
    goAbs (TypeAbstraction _ _ b) = go b

checkExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Type ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  e' <- inferExpression' e
  let inferredType = e' ^. typedType
  unlessM (matchTypes expectedTy inferredType) (throw (err inferredType))
  return (ExpressionTyped e')
  where
    err infTy =
      ErrWrongType
        ( WrongType
            { _wrongTypeExpression = e,
              _wrongTypeInferredType = infTy,
              _wrongTypeExpectedType = expectedTy
            }
        )

inferExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap ExpressionTyped . inferExpression'

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Type
lookupVar v = HashMap.lookupDefault impossible v <$> asks (^. localTypes)

checkFunctionClauseBody ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Inference] r =>
  LocalVars ->
  Type ->
  Expression ->
  Sem r Expression
checkFunctionClauseBody locals expectedTy body =
  runReader locals (checkExpression expectedTy body)

checkFunctionClause ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Inference] r =>
  FunctionInfo ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause info FunctionClause {..} = do
  let (argTys, rty) = unfoldFunType (info ^. functionInfoDef . funDefType)
      (patTys, restTys) = splitAt (length _clausePatterns) argTys
      bodyTy = foldFunType restTys rty
  if
      | length patTys /= length _clausePatterns -> impossible
      | otherwise -> do
          locals <- checkPatterns _clauseName (zipExact patTys _clausePatterns)
          let bodyTy' =
                substitution
                  ( fmap
                      (TypeIden . TypeIdenVariable)
                      (locals ^. localTyMap)
                  )
                  bodyTy
          _clauseBody' <- checkFunctionClauseBody locals bodyTy' _clauseBody
          return
            FunctionClause
              { _clauseBody = _clauseBody',
                ..
              }

checkPatterns ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  FunctionName ->
  [(FunctionArgType, Pattern)] ->
  Sem r LocalVars
checkPatterns name = execState emptyLocalVars . mapM_ (uncurry (checkPattern name))

typeOfArg :: FunctionArgType -> Type
typeOfArg a = case a of
  FunctionArgTypeAbstraction {} -> TypeUniverse
  FunctionArgTypeType ty -> ty

checkPattern ::
  forall r.
  Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars] r =>
  FunctionName ->
  FunctionArgType ->
  Pattern ->
  Sem r ()
checkPattern funName = go
  where
    go :: FunctionArgType -> Pattern -> Sem r ()
    go argTy p = do
      tyVarMap <- fmap (TypeIden . TypeIdenVariable) . (^. localTyMap) <$> get
      let ty = substitution tyVarMap (typeOfArg argTy)
          unbrace = \case
            PatternBraces b -> b
            x -> x
      case unbrace p of
        PatternWildcard {} -> return ()
        PatternBraces {} -> impossible
        PatternVariable v -> do
          modify (addType v ty)
          case argTy of
            FunctionArgTypeAbstraction (_, v') -> do
              modify (over localTyMap (HashMap.insert v' v))
            _ -> return ()
        PatternConstructorApp a -> do
          (ind, tyArgs) <- checkSaturatedInductive ty
          info <- lookupConstructor (a ^. constrAppConstructor)
          let constrInd = info ^. constructorInfoInductive
          when
            (ind /= constrInd)
            ( throw
                ( ErrWrongConstructorType
                    (WrongConstructorType (a ^. constrAppConstructor) ind constrInd funName)
                )
            )
          goConstr a tyArgs
      where
        goConstr :: ConstructorApp -> [(InductiveParameter, Type)] -> Sem r ()
        goConstr app@(ConstructorApp c ps) ctx = do
          (_, psTys) <- constructorArgTypes <$> lookupConstructor c
          let psTys' = map (substituteIndParams ctx) psTys
              expectedNum = length psTys
          let w = map FunctionArgTypeType psTys'
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
    checkSaturatedInductive :: Type -> Sem r (InductiveName, [(InductiveParameter, Type)])
    checkSaturatedInductive t = do
      (ind, args) <- viewInductiveApp t
      params <-
        (^. inductiveInfoDef . inductiveParameters)
          <$> lookupInductive ind
      let numArgs = length args
          numParams = length params
      when (numArgs < numParams) (error "unsaturated inductive type")
      when (numArgs > numParams) (error "too many arguments to inductive type")
      return (ind, zip params args)

freshHole :: Members '[Inference, NameIdGen] r => Interval -> Sem r Hole
freshHole l = do
  uid <- freshNameId
  let h = Hole uid l
  void (freshMetavar h)
  return h

-- | Returns {A : Type} → A
literalType :: Members '[NameIdGen] r => LiteralLoc -> Sem r TypedExpression
literalType l = do
  uid <- freshNameId
  let typeVar =
        Name
          { _nameText = "A",
            _nameId = uid,
            _nameKind = KNameLocal,
            _nameLoc = getLoc l,
            _nameDefined = getLoc l
          }
      type_ =
        TypeAbs
          TypeAbstraction
            { _typeAbsVar = typeVar,
              _typeAbsImplicit = Implicit,
              _typeAbsBody = TypeIden (TypeIdenVariable typeVar)
            }
  return
    TypedExpression
      { _typedType = type_,
        _typedExpression = ExpressionLiteral l
      }

inferExpression' ::
  forall r.
  Members '[Reader InfoTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference] r =>
  Expression ->
  Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionTyped t -> return t
  ExpressionLiteral l -> goLiteral l
  ExpressionFunction f -> goExpressionFunction f
  ExpressionHole h -> freshMetavar h
  where
    goExpressionFunction :: FunctionExpression -> Sem r TypedExpression
    goExpressionFunction (FunctionExpression l r) = do
      l' <- checkExpression TypeUniverse l
      r' <- checkExpression TypeUniverse r
      return (TypedExpression TypeUniverse (ExpressionFunction (FunctionExpression l' r')))
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
    inferApplication a = inferExpression' leftExp >>= helper
      where
        i = a ^. appImplicit
        leftExp = a ^. appLeft
        helper :: TypedExpression -> Sem r TypedExpression
        helper l = case l ^. typedType of
          TypeFunction f -> do
            r <- checkExpression (f ^. funLeft) (a ^. appRight)
            return
              TypedExpression
                { _typedExpression =
                    ExpressionApplication
                      Application
                        { _appLeft = ExpressionTyped l,
                          _appRight = r,
                          _appImplicit = i
                        },
                  _typedType = f ^. funRight
                }
          TypeAbs ta -> do
            r <- checkExpression TypeUniverse (a ^. appRight)
            let tr = expressionAsType' r
            return
              TypedExpression
                { _typedExpression =
                    ExpressionApplication
                      Application
                        { _appLeft = ExpressionTyped l,
                          _appRight = r,
                          _appImplicit = i
                        },
                  _typedType = substituteType1 (ta ^. typeAbsVar, tr) (ta ^. typeAbsBody)
                }
          -- When we have have an application with a hole on the left: '_@1 x'
          -- We assume that it is a type application and thus 'x' must be a type.
          -- where @2 is fresh.
          -- Not sure if this is always desirable.
          TypeHole h -> do
            q <- queryMetavar h
            case q of
              Just ty -> helper (set typedType ty l)
              Nothing -> do
                r <- checkExpression TypeUniverse (a ^. appRight)
                h' <- freshHole (getLoc h)
                let tr = expressionAsType' r
                    fun = Function tr (TypeHole h')
                unlessM (matchTypes (TypeHole h) (TypeFunction fun)) impossible
                return
                  TypedExpression
                    { _typedType = TypeHole h',
                      _typedExpression =
                        ExpressionApplication
                          Application
                            { _appLeft = ExpressionTyped l,
                              _appRight = r,
                              _appImplicit = i
                            }
                    }
          _ -> throw tyErr
            where
              tyErr :: TypeCheckerError
              tyErr =
                ErrExpectedFunctionType
                  ( ExpectedFunctionType
                      { _expectedFunctionTypeExpression = e,
                        _expectedFunctionTypeApp = leftExp,
                        _expectedFunctionTypeType = l ^. typedType
                      }
                  )

viewInductiveApp ::
  Member (Error TypeCheckerError) r =>
  Type ->
  Sem r (InductiveName, [Type])
viewInductiveApp ty = case t of
  TypeIden (TypeIdenInductive n) -> return (n, as)
  _ -> throw @TypeCheckerError (error "only inductive types can be pattern matched")
  where
    (t, as) = viewTypeApp ty

viewTypeApp :: Type -> (Type, [Type])
viewTypeApp t = case t of
  TypeApp (TypeApplication l r _) ->
    second (`snoc` r) (viewTypeApp l)
  _ -> (t, [])
