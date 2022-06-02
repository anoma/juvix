module MiniJuvix.Syntax.MicroJuvix.TypeChecker
  ( module MiniJuvix.Syntax.MicroJuvix.TypeChecker,
    module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
    module MiniJuvix.Syntax.MicroJuvix.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.Concrete.Language (LiteralLoc)
import MiniJuvix.Syntax.MicroJuvix.Error
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.LocalVars
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import MiniJuvix.Syntax.MicroJuvix.TypeChecker.Inference

entryMicroJuvixTyped ::
  Member (Error TypeCheckerError) r =>
  MicroJuvixResult ->
  Sem r MicroJuvixTypedResult
entryMicroJuvixTyped res@MicroJuvixResult {..} = do
  r <- runReader table (mapM checkModule _resultModules)
  return
    MicroJuvixTypedResult
      { _resultMicroJuvixResult = res,
        _resultModules = r
      }
  where
    table :: InfoTable
    table = buildTable _resultModules

checkModule ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
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
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
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
      TypeAny -> return ()
      TypeUniverse -> return ()
    goApp :: TypeApplication -> Sem r ()
    goApp (TypeApplication a b) = go a >> go b
    goFunction :: Function -> Sem r ()
    goFunction (Function a b) = go a >> go b
    goAbs :: TypeAbstraction -> Sem r ()
    goAbs (TypeAbstraction _ b) = go b

checkExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, Reader LocalVars, Inference] r =>
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
  Members '[Reader InfoTable, Error TypeCheckerError, Reader LocalVars, Inference] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap ExpressionTyped . inferExpression'

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Type
lookupVar v = HashMap.lookupDefault impossible v <$> asks (^. localTypes)

constructorType :: Member (Reader InfoTable) r => Name -> Sem r Type
constructorType c = do
  info <- lookupConstructor c
  let (as, bs) = constructorArgTypes info
      args =
        map FunctionArgTypeAbstraction as
          ++ map FunctionArgTypeType bs
      ind = TypeIden (TypeIdenInductive (info ^. constructorInfoInductive))
      saturatedTy =
        foldl'
          ( \t v ->
              TypeApp
                ( TypeApplication
                    t
                    (TypeIden (TypeIdenVariable v))
                )
          )
          ind
          as
  return (foldFunType args saturatedTy)

constructorArgTypes :: ConstructorInfo -> ([VarName], [Type])
constructorArgTypes i =
  ( map (^. inductiveParamName) (i ^. constructorInfoInductiveParameters),
    i ^. constructorInfoArgs
  )

checkFunctionClauseBody ::
  Members '[Reader InfoTable, Error TypeCheckerError, Inference] r =>
  LocalVars ->
  Type ->
  Expression ->
  Sem r Expression
checkFunctionClauseBody locals expectedTy body =
  runReader locals (checkExpression expectedTy body)

checkFunctionClause ::
  Members '[Reader InfoTable, Error TypeCheckerError, Inference] r =>
  FunctionInfo ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause info clause@FunctionClause {..} = do
  let (argTys, rty) = unfoldFunType (info ^. functionInfoDef . funDefType)
      (patTys, restTys) = splitAt (length _clausePatterns) argTys
      bodyTy = foldFunType restTys rty
  if
      | length patTys /= length _clausePatterns -> throw (tyErr patTys)
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
  where
    tyErr :: [FunctionArgType] -> TypeCheckerError
    tyErr patTys =
      ErrTooManyPatterns
        ( TooManyPatterns
            { _tooManyPatternsClause = clause,
              _tooManyPatternsTypes = patTys
            }
        )

checkPatterns ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  FunctionName ->
  [(FunctionArgType, Pattern)] ->
  Sem r LocalVars
checkPatterns name = execState emptyLocalVars . go
  where
    go ::
      Members '[Error TypeCheckerError, Reader InfoTable, State LocalVars] r =>
      [(FunctionArgType, Pattern)] ->
      Sem r ()
    go = mapM_ (uncurry (checkPattern name))

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
      case p of
        PatternWildcard -> return ()
        PatternVariable v -> do
          modify (addType v ty)
          case argTy of
            FunctionArgTypeAbstraction v' -> do
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
          when (expectedNum /= length ps) (throw (appErr app w))
          zipWithM_ go w ps
        appErr :: ConstructorApp -> [FunctionArgType] -> TypeCheckerError
        appErr app tys =
          ErrWrongConstructorAppArgs
            ( WrongConstructorAppArgs
                { _wrongCtorAppApp = app,
                  _wrongCtorAppTypes = tys,
                  _wrongCtorAppName = funName
                }
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

inferExpression' ::
  forall r.
  Members '[Reader InfoTable, Reader LocalVars, Error TypeCheckerError, Inference] r =>
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
    goLiteral l = return (TypedExpression TypeAny (ExpressionLiteral l))
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
        info <- lookupInductive v
        let ps = info ^. inductiveInfoDef . inductiveParameters
            kind =
              foldr
                (\p k -> TypeAbs (TypeAbstraction (p ^. inductiveParamName) k))
                TypeUniverse
                ps
        return (TypedExpression kind (ExpressionIden i))
    inferApplication :: Application -> Sem r TypedExpression
    inferApplication a = do
      let leftExp = a ^. appLeft
      l <- inferExpression' leftExp
      fun <- getFunctionType leftExp (l ^. typedType)
      case fun of
        Left ta -> do
          r <- checkExpression TypeUniverse (a ^. appRight)
          let tr = expressionAsType' r
          return
            TypedExpression
              { _typedExpression =
                  ExpressionApplication
                    Application
                      { _appLeft = ExpressionTyped l,
                        _appRight = r
                      },
                _typedType = substituteType1 (ta ^. typeAbsVar, tr) (ta ^. typeAbsBody)
              }
        Right f -> do
          r <- checkExpression (f ^. funLeft) (a ^. appRight)
          return
            TypedExpression
              { _typedExpression =
                  ExpressionApplication
                    Application
                      { _appLeft = ExpressionTyped l,
                        _appRight = r
                      },
                _typedType = f ^. funRight
              }
      where
        getFunctionType :: Expression -> Type -> Sem r (Either TypeAbstraction Function)
        getFunctionType appExp t = case t of
          TypeFunction f -> return (Right f)
          TypeAbs f -> return (Left f)
          _ -> throw tyErr
          where
            tyErr :: TypeCheckerError
            tyErr =
              ErrExpectedFunctionType
                ( ExpectedFunctionType
                    { _expectedFunctionTypeExpression = e,
                      _expectedFunctionTypeApp = appExp,
                      _expectedFunctionTypeType = t
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
  TypeApp (TypeApplication l r) ->
    second (`snoc` r) (viewTypeApp l)
  _ -> (t, [])
