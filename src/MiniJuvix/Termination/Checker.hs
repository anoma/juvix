module MiniJuvix.Termination.Checker
  ( module MiniJuvix.Termination.Checker,
    module MiniJuvix.Termination.FunctionCall,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.InfoTable
import MiniJuvix.Syntax.Abstract.Language.Extra
import MiniJuvix.Syntax.Concrete.Scoped.Name (unqualifiedSymbol)
import MiniJuvix.Termination.FunctionCall
import MiniJuvix.Termination.Types

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
