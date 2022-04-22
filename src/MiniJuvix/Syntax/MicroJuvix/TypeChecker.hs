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
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.LocalVars
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import Polysemy.Error (fromEither)

entryMicroJuvixTyped ::
  (Member (Error TypeCheckerError) r) =>
  MicroJuvixResult ->
  Sem r MicroJuvixTypedResult
entryMicroJuvixTyped res@MicroJuvixResult {..} = do
  r <- fromEither (mapM checkModule _resultModules)
  return
    MicroJuvixTypedResult
      { _resultMicroJuvixResult = res,
        _resultModules = r
      }

checkModule :: Module -> Either TypeCheckerError Module
checkModule m =
  run $ runError $ runReader (buildTable m) (checkModule' m)

checkModule' ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  Module ->
  Sem r Module
checkModule' Module {..} = do
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

checkStatement ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  info <- lookupFunction _funDefName
  _funDefClauses' <- mapM (checkFunctionClause info) _funDefClauses
  return
    FunctionDef
      { _funDefClauses = _funDefClauses',
        ..
      }

checkExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, Reader LocalVars] r =>
  Type ->
  Expression ->
  Sem r Expression
checkExpression t e = do
  e' <- inferExpression' e
  let inferredType = e' ^. typedType
  unlessM (matchTypes t inferredType) (throw (err inferredType))
  return (ExpressionTyped e')
  where
    err infTy =
      ErrWrongType
        ( WrongType
            { _wrongTypeExpression = e,
              _wrongTypeInferredType = infTy,
              _wrongTypeExpectedType = t
            }
        )

matchTypes ::
  Members '[Reader InfoTable, Reader LocalVars] r =>
  Type ->
  Type ->
  Sem r Bool
matchTypes a b = do
  areAlphaEq <- alphaEq a b
  return $
    isAny a || isAny b || areAlphaEq
  where
    isAny = \case
      TypeAny -> True
      _ -> False

-- | Alpha equivalence
alphaEq :: Type -> Type -> Sem r Bool
alphaEq ty = runReader ini . go ty
  where
    ini :: HashMap VarName VarName
    ini = mempty
    go ::
      forall r.
      Members '[Reader (HashMap VarName VarName)] r =>
      Type ->
      Type ->
      Sem r Bool
    go a' b' = case (a', b') of
      (TypeIden a, TypeIden b) -> goIden a b
      (TypeApp a, TypeApp b) -> goApp a b
      (TypeAbs a, TypeAbs b) -> goAbs a b
      (TypeFunction a, TypeFunction b) -> goFunction a b
      (TypeUniverse, TypeUniverse) -> return True
      -- TODO TypeAny should match anything?
      (TypeAny, TypeAny) -> return True
      -- TODO is the final wildcard bad style?
      -- what if more Type constructors are added
      _ -> return False
      where
        goIden :: TypeIden -> TypeIden -> Sem r Bool
        goIden ia ib = case (ia, ib) of
          (TypeIdenInductive a, TypeIdenInductive b) -> return (a == b)
          (TypeIdenAxiom a, TypeIdenAxiom b) -> return (a == b)
          (TypeIdenVariable a, TypeIdenVariable b) -> do
            mappedEq <- (== Just b) . HashMap.lookup a <$> ask
            return (a == b || mappedEq)
          _ -> return False
        goApp :: TypeApplication -> TypeApplication -> Sem r Bool
        goApp (TypeApplication f x) (TypeApplication f' x') = andM [go f f', go x x']
        goFunction :: Function -> Function -> Sem r Bool
        goFunction (Function l r) (Function l' r') = andM [go l l', go r r']
        goAbs :: TypeAbstraction -> TypeAbstraction -> Sem r Bool
        goAbs (TypeAbstraction v1 r) (TypeAbstraction v2 r') =
          local (HashMap.insert v1 v2) (go r r')

inferExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, Reader LocalVars] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap ExpressionTyped . inferExpression'

lookupConstructor :: Member (Reader InfoTable) r => Name -> Sem r ConstructorInfo
lookupConstructor f = HashMap.lookupDefault impossible f <$> asks _infoConstructors

lookupInductive :: Member (Reader InfoTable) r => InductiveName -> Sem r InductiveInfo
lookupInductive f = HashMap.lookupDefault impossible f <$> asks _infoInductives

lookupFunction :: Member (Reader InfoTable) r => Name -> Sem r FunctionInfo
lookupFunction f = HashMap.lookupDefault impossible f <$> asks _infoFunctions

lookupAxiom :: Member (Reader InfoTable) r => Name -> Sem r AxiomInfo
lookupAxiom f = HashMap.lookupDefault impossible f <$> asks _infoAxioms

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Type
lookupVar v = HashMap.lookupDefault impossible v <$> asks _localTypes

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

-- | [a, b] c ==> a -> (b -> c)
foldFunType :: [FunctionArgType] -> Type -> Type
foldFunType l r = case l of
  [] -> r
  (a : as) ->
    let r' = foldFunType as r
     in case a of
          FunctionArgTypeAbstraction v -> TypeAbs (TypeAbstraction v r')
          FunctionArgTypeType t -> TypeFunction (Function t r')

-- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Type -> ([FunctionArgType], Type)
unfoldFunType t = case t of
  TypeFunction (Function l r) -> first (FunctionArgTypeType l :) (unfoldFunType r)
  TypeAbs (TypeAbstraction var r) -> first (FunctionArgTypeAbstraction var :) (unfoldFunType r)
  _ -> ([], t)

checkFunctionClause ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  FunctionInfo ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause info clause@FunctionClause {..} = do
  let (argTys, rty) = unfoldFunType (info ^. functionInfoType)
      (patTys, restTys) = splitAt (length _clausePatterns) argTys
      bodyTy = foldFunType restTys rty
  if
      -- TODO consider zip exact
      | length patTys /= length _clausePatterns -> throw (tyErr patTys)
      | otherwise -> do
          locals <- checkPatterns _clauseName (zip patTys _clausePatterns)
          let bodyTy' =
                substitution
                  ( fmap
                      (TypeIden . TypeIdenVariable)
                      (locals ^. localTyMap)
                  )
                  bodyTy
          _clauseBody' <-
            runReader locals (checkExpression bodyTy' _clauseBody)
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

substitutionArg :: VarName -> VarName -> FunctionArgType -> FunctionArgType
substitutionArg from v a = case a of
  FunctionArgTypeAbstraction {} -> a
  FunctionArgTypeType ty ->
    FunctionArgTypeType
      (substitution1 (from, TypeIden (TypeIdenVariable v)) ty)

substitution1 :: (VarName, Type) -> Type -> Type
substitution1 = substitution . uncurry HashMap.singleton

substitution :: HashMap VarName Type -> Type -> Type
substitution m = go
  where
    go :: Type -> Type
    go = \case
      TypeIden i -> goIden i
      TypeApp a -> TypeApp (goApp a)
      TypeAbs a -> TypeAbs (goAbs a)
      TypeFunction f -> TypeFunction (goFunction f)
      TypeUniverse -> TypeUniverse
      TypeAny -> TypeAny
    goApp :: TypeApplication -> TypeApplication
    goApp (TypeApplication l r) = TypeApplication (go l) (go r)
    goAbs :: TypeAbstraction -> TypeAbstraction
    goAbs (TypeAbstraction v b) = TypeAbstraction v (go b)
    goFunction :: Function -> Function
    goFunction (Function l r) = Function (go l) (go r)
    goIden :: TypeIden -> Type
    goIden i = case i of
      TypeIdenInductive {} -> TypeIden i
      TypeIdenAxiom {} -> TypeIden i
      TypeIdenVariable v -> case HashMap.lookup v m of
        Just ty -> ty
        Nothing -> TypeIden i

substituteIndParams :: [(InductiveParameter, Type)] -> Type -> Type
substituteIndParams = substitution . HashMap.fromList . map (first (^. inductiveParamName))

checkPattern ::
  forall r.
  Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars] r =>
  FunctionName ->
  FunctionArgType ->
  Pattern ->
  Sem r ()
checkPattern funName type_ pat = go type_ pat
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

-- | The expression is assumed to be of type TypeUniverse.
-- If the assumption holds, it should never fail.
expressionAsType :: Expression -> Type
expressionAsType = go
  where
    go = \case
      ExpressionIden i -> TypeIden (goIden i)
      ExpressionApplication a -> TypeApp (goApp a)
      ExpressionLiteral {} -> impossible
      ExpressionTyped e -> go (e ^. typedExpression)
    goIden :: Iden -> TypeIden
    goIden = \case
      IdenFunction {} -> impossible
      IdenConstructor {} -> impossible
      IdenVar v -> TypeIdenVariable v
      IdenAxiom a -> TypeIdenAxiom a
      IdenInductive i -> TypeIdenInductive i
    goApp :: Application -> TypeApplication
    goApp (Application l r) = TypeApplication (go l) (go r)

inferExpression' ::
  forall r.
  Members '[Reader InfoTable, Reader LocalVars, Error TypeCheckerError] r =>
  Expression ->
  Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionTyped t -> return t
  ExpressionLiteral l -> goLiteral l
  where
    goLiteral :: LiteralLoc -> Sem r TypedExpression
    goLiteral l = return (TypedExpression TypeAny (ExpressionLiteral l))
    inferIden :: Iden -> Sem r TypedExpression
    inferIden i = case i of
      IdenFunction fun -> do
        info <- lookupFunction fun
        return (TypedExpression (info ^. functionInfoType) (ExpressionIden i))
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
          let tr = expressionAsType r
          return
            TypedExpression
              { _typedExpression =
                  ExpressionApplication
                    Application
                      { _appLeft = ExpressionTyped l,
                        _appRight = r
                      },
                _typedType = substitution1 (ta ^. typeAbsVar, tr) (ta ^. typeAbsBody)
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
  TypeAny {} -> (t, [])
  TypeUniverse {} -> (t, [])
  TypeAbs {} -> (t, [])
  TypeFunction {} -> (t, [])
  TypeIden {} -> (t, [])
