module MiniJuvix.Syntax.MicroJuvix.TypeChecker where
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import qualified Data.HashMap.Strict as HashMap
import MiniJuvix.Syntax.MicroJuvix.Error

newtype LocalVars = LocalVars {
  _localTypes :: HashMap VarName Type
  }
  deriving newtype (Semigroup, Monoid)
makeLenses ''LocalVars

checkModule :: Module -> Either [TypeCheckerError] Module
checkModule m = run $ do
  (es, checkedModule) <- runOutputList $ runReader (buildTable m) (checkModule' m)
  return $ case es of
    [] -> Right checkedModule
    _ -> Left es

checkModule' :: Members '[Reader InfoTable, Output TypeCheckerError] r =>
  Module -> Sem r Module
checkModule' Module {..} = do
  _moduleBody' <- checkModuleBody _moduleBody
  return Module {
    _moduleBody = _moduleBody',
    ..
    }

checkModuleBody :: Members '[Reader InfoTable, Output TypeCheckerError] r =>
  ModuleBody -> Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return ModuleBody {
    _moduleStatements = _moduleStatements'
    }

checkStatement :: Members '[Reader InfoTable, Output TypeCheckerError] r =>
  Statement -> Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementAxiom {} -> return s

checkFunctionDef :: Members '[Reader InfoTable, Output TypeCheckerError] r =>
  FunctionDef -> Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  info <- lookupFunction _funDefName
  _funDefClauses' <- mapM (checkFunctionClause info) _funDefClauses
  return FunctionDef {
    _funDefClauses = _funDefClauses',
    ..
    }

checkExpression :: Members '[Reader InfoTable, Output TypeCheckerError, Reader LocalVars] r =>
  Type -> Expression -> Sem r Expression
checkExpression t e = do
  t' <- inferExpression' e
  let inferredType = t' ^. typedType
  if (t /= inferredType)
    then output (err inferredType) >> return e
    else return (ExpressionTyped t')
  where
    err infTy = (ErrWrongType (WrongType { _wrongTypeExpression = e,
                                           _wrongTypeInferredType = infTy,
                                           _wrongTypeExpectedType = t}))

inferExpression :: Members '[Reader InfoTable, Output TypeCheckerError, Reader LocalVars] r =>
   Expression -> Sem r Expression
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
  TypeFunction (Function l r) -> first (l:) (unfoldFunType r)
  _ -> ([], t)

checkFunctionClause :: forall r. Members '[Reader InfoTable, Output TypeCheckerError] r =>
  FunctionInfo -> FunctionClause -> Sem r FunctionClause
checkFunctionClause info clause@FunctionClause{..} = do
  let (argTys, rty) = unfoldFunType (info ^. functionInfoType)
      (patTys, restTys) = splitAt (length _clausePatterns) argTys
      bodyTy = foldFunType restTys rty
  if (length patTys /= length _clausePatterns)
    then output (tyErr patTys) >> return clause
    else do
      locals <- mconcat <$> zipWithM checkPattern patTys _clausePatterns
      clauseBody' <- runReader locals (checkExpression bodyTy _clauseBody)
      return FunctionClause {
        _clauseBody = clauseBody',
        ..
        }
  where
    tyErr :: [Type] -> TypeCheckerError
    tyErr patTys = (ErrTooManyPatterns (TooManyPatterns {_tooManyPatternsClause = clause,
                                                          _tooManyPatternsTypes = patTys}))

checkPattern :: forall r. Members '[Reader InfoTable, Output TypeCheckerError] r =>
  Type -> Pattern -> Sem r LocalVars
checkPattern type_ pat = LocalVars . HashMap.fromList <$> go type_ pat
  where
  go :: Type -> Pattern -> Sem r [(VarName, Type)]
  go ty p = case p of
    PatternWildcard -> return []
    PatternVariable v -> return [(v, ty)]
    PatternConstructorApp a -> do
      info <- lookupConstructor (a ^. constrAppConstructor)
      let inductiveTy = TypeIden (TypeIdenInductive (info ^. constructorInfoInductive))
      _ <- when (inductiveTy /= ty) (output (ErrWrongConstructorType (WrongConstructorType (a ^. constrAppConstructor) ty inductiveTy)))
      goConstr a
    where
    goConstr :: ConstructorApp -> Sem r [(VarName, Type)]
    goConstr app@(ConstructorApp c ps) = do
      tys <- (^. constructorInfoArgs) <$> lookupConstructor c
      _ <- when (length tys /= length ps) (output (appErr app tys))
      concat <$> zipWithM go tys ps
    appErr :: ConstructorApp -> [Type] -> TypeCheckerError
    appErr app tys = ErrWrongConstructorAppArgs (WrongConstructorAppArgs { _wrongCtorAppApp = app,
                                                                           _wrongCtorAppTypes = tys})

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

inferExpression' :: forall r. Members '[Reader InfoTable, Output TypeCheckerError, Reader LocalVars] r =>
   Expression -> Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionTyped {} -> impossible
  ExpressionLiteral l -> goLiteral l
  where
  goLiteral :: Literal -> Sem r TypedExpression
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
    case getFunctionType leftExp (l ^. typedType) of
      Left tyErr -> output tyErr >> (return (TypedExpression TypeAny e))
      Right fun -> do
        r <- checkExpression (fun ^. funLeft) (a ^. appRight)
        return TypedExpression {
          _typedExpression = ExpressionApplication Application {
              _appLeft = ExpressionTyped l,
              _appRight = r
              },
          _typedType = fun ^. funRight
          }
    where
      getFunctionType :: Expression -> Type -> Either TypeCheckerError Function
      getFunctionType appExp t = case t of
        TypeFunction f -> Right f
        _ -> Left tyErr
        where
          tyErr :: TypeCheckerError
          tyErr = (ErrExpectedFunctionType (ExpectedFunctionType { _expectedFunctionTypeExpression = e,
                                                                   _expectedFunctionTypeApp = appExp,
                                                                   _expectedFunctionTypeType = t}))
