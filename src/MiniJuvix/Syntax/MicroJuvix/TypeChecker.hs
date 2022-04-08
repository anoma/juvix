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
  (Member (Error TypeCheckerErrors) r) =>
  MicroJuvixResult ->
  Sem r MicroJuvixTypedResult
entryMicroJuvixTyped res@MicroJuvixResult {..} = do
  r <- fromEither (mapM checkModule _resultModules)
  return
    MicroJuvixTypedResult
      { _resultMicroJuvixResult = res,
        _resultModules = r
      }

checkModule :: Module -> Either TypeCheckerErrors Module
checkModule m = run $ do
  (es, checkedModule) <- runOutputList $ runReader (buildTable m) (checkModule' m)
  return $ case nonEmpty es of
    Nothing -> Right checkedModule
    Just xs -> Left (TypeCheckerErrors {_unTypeCheckerErrors = xs})

checkModule' ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
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
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkStatement ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
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
  t' <- inferExpression' e
  let inferredType = t' ^. typedType
  unlessM (matchTypes t inferredType) (throw (err inferredType))
  return (ExpressionTyped t')
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
  Members '[Reader InfoTable] r =>
  Type ->
  Type ->
  Sem r Bool
matchTypes a b = do
  a' <- normalizeType a
  b' <- normalizeType b
  return $
    a' == TypeAny || b' == TypeAny || a' == b'

inferExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, Reader LocalVars] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap ExpressionTyped . inferExpression'

lookupConstructor :: Member (Reader InfoTable) r => Name -> Sem r ConstructorInfo
lookupConstructor f = HashMap.lookupDefault impossible f <$> asks _infoConstructors

lookupFunction :: Member (Reader InfoTable) r => Name -> Sem r FunctionInfo
lookupFunction f = HashMap.lookupDefault impossible f <$> asks _infoFunctions

lookupAxiom :: Member (Reader InfoTable) r => Name -> Sem r AxiomInfo
lookupAxiom f = HashMap.lookupDefault impossible f <$> asks _infoAxioms

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Type
lookupVar v = HashMap.lookupDefault impossible v <$> asks _localTypes

constructorType :: Member (Reader InfoTable) r => Name -> Sem r Type
constructorType c = do
  info <- lookupConstructor c
  let r = TypeIden (TypeIdenInductive (info ^. constructorInfoInductive))
  return (foldFunType (info ^. constructorInfoArgs) r)

-- | [a, b] c ==> a -> (b -> c)
foldFunType :: [Type] -> Type -> Type
foldFunType l r = case l of
  [] -> r
  (a : as) -> TypeFunction (Function a (foldFunType as r))

-- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Type -> ([Type], Type)
unfoldFunType t = case t of
  TypeFunction (Function l r) -> first (l :) (unfoldFunType r)
  _ -> ([], t)

checkFunctionClause ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  FunctionInfo ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause info clause@FunctionClause {..} = do
  let (argTys, rty) = unfoldFunType (info ^. functionInfoType)
      (patTys, restTys) = splitAt (length _clausePatterns) argTys
      bodyTy = foldFunType restTys rty
  if length patTys /= length _clausePatterns
    then output (tyErr patTys) $> clause
    else do
      locals <- mconcat <$> zipWithM (checkPattern _clauseName) patTys _clausePatterns
      eclauseBody <- runError @TypeCheckerError $ runReader locals (checkExpression bodyTy _clauseBody)
      _clauseBody' <- case eclauseBody of
        Left err -> output err $> _clauseBody
        Right r -> return r
      return
        FunctionClause
          { _clauseBody = _clauseBody',
            ..
          }
  where
    tyErr :: [Type] -> TypeCheckerError
    tyErr patTys =
      ErrTooManyPatterns
        ( TooManyPatterns
            { _tooManyPatternsClause = clause,
              _tooManyPatternsTypes = patTys
            }
        )

checkPattern ::
  forall r.
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  FunctionName ->
  Type ->
  Pattern ->
  Sem r LocalVars
checkPattern funName type_ pat = LocalVars . HashMap.fromList <$> go type_ pat
  where
    go :: Type -> Pattern -> Sem r [(VarName, Type)]
    go ty p = case p of
      PatternWildcard -> return []
      PatternVariable v -> return [(v, ty)]
      PatternConstructorApp a -> do
        info <- lookupConstructor (a ^. constrAppConstructor)
        let inductiveTy = TypeIden (TypeIdenInductive (info ^. constructorInfoInductive))
        when (inductiveTy /= ty) (output (ErrWrongConstructorType (WrongConstructorType (a ^. constrAppConstructor) ty inductiveTy funName)))
        goConstr a
      where
        goConstr :: ConstructorApp -> Sem r [(VarName, Type)]
        goConstr app@(ConstructorApp c ps) = do
          tys <- (^. constructorInfoArgs) <$> lookupConstructor c
          when (length tys /= length ps) (output (appErr app tys))
          concat <$> zipWithM go tys ps
        appErr :: ConstructorApp -> [Type] -> TypeCheckerError
        appErr app tys =
          ErrWrongConstructorAppArgs
            ( WrongConstructorAppArgs
                { _wrongCtorAppApp = app,
                  _wrongCtorAppTypes = tys,
                  _wrongCtorAppName = funName
                }
            )

-- TODO currently equivalent to id
normalizeType :: forall r. Members '[Reader InfoTable] r => Type -> Sem r Type
normalizeType t = case t of
  TypeAny -> return TypeAny
  TypeUniverse -> return TypeUniverse
  TypeFunction f -> TypeFunction <$> normalizeFunction f
  TypeIden i -> normalizeIden i
  where
    normalizeIden :: TypeIden -> Sem r Type
    normalizeIden i = case i of
      TypeIdenInductive {} -> return (TypeIden i)
      TypeIdenAxiom {} -> return (TypeIden i)
    normalizeFunction :: Function -> Sem r Function
    normalizeFunction (Function l r) = do
      l' <- normalizeType l
      r' <- normalizeType r
      return (Function l' r')

inferExpression' ::
  forall r.
  Members '[Reader InfoTable, Reader LocalVars, Error TypeCheckerError] r =>
  Expression ->
  Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionTyped {} -> impossible
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
    inferApplication :: Application -> Sem r TypedExpression
    inferApplication a = do
      let leftExp = a ^. appLeft
      l <- inferExpression' leftExp
      fun <- getFunctionType leftExp (l ^. typedType)
      r <- checkExpression (fun ^. funLeft) (a ^. appRight)
      return
        TypedExpression
          { _typedExpression =
              ExpressionApplication
                Application
                  { _appLeft = ExpressionTyped l,
                    _appRight = r
                  },
            _typedType = fun ^. funRight
          }
      where
        getFunctionType :: Expression -> Type -> Sem r Function
        getFunctionType appExp t = case t of
          TypeFunction f -> return f
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
