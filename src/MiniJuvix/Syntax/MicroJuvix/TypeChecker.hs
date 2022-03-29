module MiniJuvix.Syntax.MicroJuvix.TypeChecker where
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language
import qualified Data.HashMap.Strict as HashMap

type Err = Text

newtype LocalVars = LocalVars {
  _localTypes :: HashMap VarName Type
  }
  deriving newtype (Semigroup, Monoid)
makeLenses ''LocalVars

checkModule :: Module -> Either Err Module
checkModule m = run $ runError $ runReader (buildTable m) (checkModule' m)

buildTable :: Module -> InfoTable
buildTable m = InfoTable {..}
  where
   _infoConstructors :: HashMap Name ConstructorInfo
   _infoConstructors = HashMap.fromList
     [ (c ^. constructorName, ConstructorInfo args ind) |
       StatementInductive d <- ss,
       let ind = d ^. inductiveName,
       c <- d ^. inductiveConstructors,
       let args = c ^. constructorParameters
     ]
   _infoFunctions :: HashMap Name FunctionInfo
   _infoFunctions = HashMap.fromList
     [ (f ^. funDefName, FunctionInfo (f ^. funDefTypeSig)) |
       StatementFunction f <- ss]
   ss = m ^. moduleBody ^. moduleStatements

checkModule' :: Members '[Reader InfoTable, Error Err] r =>
  Module -> Sem r Module
checkModule' Module {..} = do
  _moduleBody' <- checkModuleBody _moduleBody
  return Module {
    _moduleBody = _moduleBody',
    ..
    }

checkModuleBody :: Members '[Reader InfoTable, Error Err] r =>
  ModuleBody -> Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return ModuleBody {
    _moduleStatements = _moduleStatements'
    }

checkStatement :: Members '[Reader InfoTable, Error Err] r =>
  Statement -> Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s -- TODO is checking inductives needed?

checkFunctionDef :: Members '[Reader InfoTable, Error Err] r =>
  FunctionDef -> Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  info <- lookupFunction _funDefName
  _funDefClauses' <- mapM (checkFunctionClause info) _funDefClauses
  return FunctionDef {
    _funDefClauses = _funDefClauses',
    ..
    }

checkExpression :: Members '[Reader InfoTable, Error Err, Reader LocalVars] r =>
  Type -> Expression -> Sem r Expression
checkExpression t e = do
  t' <- inferExpression' e
  when (t /= t' ^. typedType) (throwErr "wrong type")
  return (ExpressionTyped t')

inferExpression :: Members '[Reader InfoTable, Error Err, Reader LocalVars] r =>
   Expression -> Sem r Expression
inferExpression = fmap ExpressionTyped . inferExpression'

lookupConstructor :: Member (Reader InfoTable) r => Name -> Sem r ConstructorInfo
lookupConstructor f = HashMap.lookupDefault impossible f <$> asks _infoConstructors

lookupFunction :: Member (Reader InfoTable) r => Name -> Sem r FunctionInfo
lookupFunction f = HashMap.lookupDefault impossible f <$> asks _infoFunctions

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

-- |  a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Type -> ([Type], Type)
unfoldFunType t = case t of
  TypeIden {} -> ([], t)
  TypeFunction (Function l r) -> first (l:) (unfoldFunType r)

throwErr :: Members '[Error Err] r => Err -> Sem r a
throwErr = throw

inferExpression' :: forall r. Members '[Reader InfoTable, Error Err, Reader LocalVars] r =>
   Expression -> Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> checkIden i
  ExpressionApplication a -> checkApplication a
  ExpressionTyped {} -> impossible
  where
  checkIden :: Iden -> Sem r TypedExpression
  checkIden i = case i of
    IdenFunction fun -> do
      info <- lookupFunction fun
      return (TypedExpression (info ^. functionInfoType) (ExpressionIden i))
    IdenConstructor c -> do
      ty <- constructorType c
      return (TypedExpression ty (ExpressionIden i))
    IdenVar v -> do
      ty <- lookupVar v
      return (TypedExpression ty (ExpressionIden i))
  checkApplication :: Application -> Sem r TypedExpression
  checkApplication a = do
    l <- inferExpression' (a ^. appLeft)
    fun <- getFunctionType (l ^. typedType)
    r <- checkExpression (fun ^. funLeft) (a ^. appRight)
    return TypedExpression {
      _typedExpression = ExpressionApplication Application {
          _appLeft = ExpressionTyped l,
          _appRight = r
          },
      _typedType = fun ^. funRight
      }
  getFunctionType :: Type -> Sem r Function
  getFunctionType t = case t of
    TypeFunction f -> return f
    _ -> throwErr "expected function type"

checkFunctionClause :: forall r. Members '[Reader InfoTable, Error Err] r =>
  FunctionInfo -> FunctionClause -> Sem r FunctionClause
checkFunctionClause info FunctionClause{..} = do
  let (argTys, rty) = unfoldFunType (info ^. functionInfoType)
      (patTys, restTys) = splitAt (length _clausePatterns) argTys
      bodyTy = foldFunType restTys rty
  when (length patTys /= length _clausePatterns) (throwErr "wrong number of patterns")
  locals <- mconcat <$> zipWithM checkPattern patTys _clausePatterns
  clauseBody' <- runReader locals (checkExpression bodyTy _clauseBody)
  return FunctionClause {
    _clauseBody = clauseBody',
    ..
    }

checkPattern :: forall r. Members '[Reader InfoTable, Error Err] r =>
  Type -> Pattern -> Sem r LocalVars
checkPattern type_ pat = LocalVars . HashMap.fromList <$> go type_ pat
  where
  go :: Type -> Pattern -> Sem r [(VarName, Type)]
  go ty p = case p of
    PatternWildcard -> return []
    PatternVariable v -> return [(v, ty)]
    PatternConstructorApp a -> goConstr a
    where
    goConstr :: ConstructorApp -> Sem r [(VarName, Type)]
    goConstr (ConstructorApp c ps) = do
      tys <- (^. constructorInfoArgs) <$> lookupConstructor c
      when (length tys /= length ps) (throwErr "wrong number of arguments in constructor app")
      concat <$> zipWithM go tys ps
