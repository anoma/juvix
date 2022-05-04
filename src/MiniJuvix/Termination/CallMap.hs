module MiniJuvix.Termination.CallMap
  ( module MiniJuvix.Termination.Types,
    module MiniJuvix.Termination.CallMap,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.InfoTable
import MiniJuvix.Syntax.Abstract.Language.Extra
import MiniJuvix.Syntax.Concrete.Scoped.Name (unqualifiedSymbol)
import MiniJuvix.Termination.Types

-- | i = SizeInfo [v] ⇔ v is smaller than argument i of the caller function.
-- Indexes are 0 based
data SizeInfo = SizeInfo
  { _sizeSmaller :: HashMap VarName Int,
    _sizeEqual :: [Pattern]
  }

viewCall ::
  forall r.
  Members '[Reader SizeInfo] r =>
  Expression ->
  Sem r (Maybe FunCall)
viewCall = \case
  ExpressionApplication (Application f x) -> do
    c <- viewCall f
    x' <- callArg
    return $ over callArgs (`snoc` x') <$> c
    where
      callArg :: Sem r (CallRow, Expression)
      callArg = do
        lt <- lessThan
        eq <- equalTo
        return (CallRow (_callRow lt `mplus` _callRow eq), x)
        where
          lessThan :: Sem r CallRow
          lessThan = case x of
            ExpressionIden (IdenVar v) -> do
              s :: Maybe Int <- asks (HashMap.lookup v . _sizeSmaller)
              return $ case s of
                Nothing -> CallRow Nothing
                Just s' -> CallRow (Just (s', RLe))
            _ -> return (CallRow Nothing)
          equalTo :: Sem r CallRow
          equalTo = do
            case viewExpressionAsPattern x of
              Just x' -> do
                s <- asks (elemIndex x' . _sizeEqual)
                return $ case s of
                  Nothing -> CallRow Nothing
                  Just s' -> CallRow (Just (s', REq))
              _ -> return (CallRow Nothing)
  ExpressionIden (IdenFunction x) ->
    return (Just (singletonCall x))
  _ -> return Nothing
  where
    singletonCall :: FunctionRef -> FunCall
    singletonCall r = FunCall r []

addCall :: FunctionRef -> FunCall -> CallMap -> CallMap
addCall fun c = over callMap (HashMap.alter (Just . insertCall c) fun)
  where
    insertCall ::
      FunCall ->
      Maybe (HashMap FunctionRef [FunCall]) ->
      HashMap FunctionRef [FunCall]
    insertCall f = \case
      Nothing -> singl f
      Just m' -> addFunCall f m'

    singl :: FunCall -> HashMap FunctionRef [FunCall]
    singl f = HashMap.singleton (f ^. callRef) [f]

    addFunCall ::
      FunCall ->
      HashMap FunctionRef [FunCall] ->
      HashMap FunctionRef [FunCall]
    addFunCall fc = HashMap.insertWith (flip (<>)) (fc ^. callRef) [fc]

registerCall ::
  Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r =>
  FunCall ->
  Sem r ()
registerCall c = do
  fun <- ask
  modify (addCall fun c)

buildCallMap :: InfoTable -> TopModule -> CallMap
buildCallMap infotable = run . execState mempty . runReader infotable . checkModule

checkModule ::
  Members '[State CallMap, Reader InfoTable] r =>
  TopModule ->
  Sem r ()
checkModule m = checkModuleBody (m ^. moduleBody)

checkModuleBody :: Members '[State CallMap, Reader InfoTable] r => ModuleBody -> Sem r ()
checkModuleBody body = do
  mapM_ checkFunctionDef moduleFunctions
  mapM_ checkLocalModule moduleLocalModules
  where
    moduleFunctions = [f | StatementFunction f <- body ^. moduleStatements]
    moduleLocalModules = [f | StatementLocalModule f <- body ^. moduleStatements]

checkLocalModule :: Members '[State CallMap, Reader InfoTable] r => LocalModule -> Sem r ()
checkLocalModule m = checkModuleBody (m ^. moduleBody)

checkFunctionDef ::
  Members '[State CallMap, Reader InfoTable] r =>
  FunctionDef ->
  Sem r ()
checkFunctionDef FunctionDef {..} =
  runReader (FunctionRef (unqualifiedSymbol _funDefName)) $ do
    checkTypeSignature _funDefTypeSig
    mapM_ checkFunctionClause _funDefClauses

checkTypeSignature ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable] r =>
  Expression ->
  Sem r ()
checkTypeSignature = runReader (emptySizeInfo :: SizeInfo) . checkExpression

emptySizeInfo :: SizeInfo
emptySizeInfo =
  SizeInfo
    { _sizeEqual = mempty,
      _sizeSmaller = mempty
    }

mkSizeInfo :: [Pattern] -> SizeInfo
mkSizeInfo ps = SizeInfo {..}
  where
    _sizeEqual = ps
    _sizeSmaller =
      HashMap.fromList
        [ (v, i) | (i, p) <- zip [0 ..] ps, v <- smallerPatternVariables p
        ]

checkFunctionClause ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable] r =>
  FunctionClause ->
  Sem r ()
checkFunctionClause FunctionClause {..} =
  runReader (mkSizeInfo _clausePatterns) $ checkExpression _clauseBody

checkExpression ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  Expression ->
  Sem r ()
checkExpression e =
  viewCall e >>= \case
    Just c -> do
      registerCall c
      mapM_ (checkExpression . snd) (c ^. callArgs)
    Nothing -> case e of
      ExpressionApplication a -> checkApplication a
      ExpressionFunction f -> checkFunction f
      ExpressionIden {} -> return ()
      ExpressionUniverse {} -> return ()
      ExpressionLiteral {} -> return ()

checkApplication ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  Application ->
  Sem r ()
checkApplication (Application l r) = do
  checkExpression l
  checkExpression r

checkFunction ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  Function ->
  Sem r ()
checkFunction (Function l r) = do
  checkFunctionParameter l
  checkExpression r

checkFunctionParameter ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  FunctionParameter ->
  Sem r ()
checkFunctionParameter p = checkExpression (p ^. paramType)
