module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Checker
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Checker,
    module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.FunctionCall,
    module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error,
  )
where

import Data.HashMap.Internal.Strict qualified as HashMap
import Juvix.Compiler.Abstract.Data.InfoTable as Abstract
import Juvix.Compiler.Abstract.Language as Abstract
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.FunctionCall
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.LexOrder
import Juvix.Prelude

checkTermination ::
  Members '[Error TerminationError] r =>
  Abstract.TopModule ->
  Abstract.InfoTable ->
  Sem r ()
checkTermination topModule infotable = do
  let callmap = buildCallMap infotable topModule
      completeGraph = completeCallGraph callmap
      rEdges = reflexiveEdges completeGraph
      recBehav = map recursiveBehaviour rEdges
  forM_ recBehav $ \r -> do
    let funName = r ^. recursiveBehaviourFun
        funRef = Abstract.FunctionRef funName
        funInfo = HashMap.lookupDefault impossible funRef (infotable ^. Abstract.infoFunctions)
        markedTerminating = funInfo ^. (Abstract.functionInfoDef . Abstract.funDefTerminating)
    if
        | markedTerminating -> return ()
        | otherwise ->
            case findOrder r of
              Nothing -> throw (ErrNoLexOrder (NoLexOrder funName))
              Just _ -> return ()

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
  runReader (FunctionRef _funDefName) $ do
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
      ExpressionHole {} -> return ()
      ExpressionUniverse {} -> return ()
      ExpressionLiteral {} -> return ()

checkApplication ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  Application ->
  Sem r ()
checkApplication (Application l r _) = do
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
