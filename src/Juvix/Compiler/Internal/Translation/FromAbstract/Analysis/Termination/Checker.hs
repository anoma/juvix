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
buildCallMap infotable = run . execState mempty . runReader infotable . scanModule

scanModule ::
  Members '[State CallMap, Reader InfoTable] r =>
  TopModule ->
  Sem r ()
scanModule m = scanModuleBody (m ^. moduleBody)

scanModuleBody :: Members '[State CallMap, Reader InfoTable] r => ModuleBody -> Sem r ()
scanModuleBody body = do
  mapM_ scanFunctionDef moduleFunctions
  mapM_ scanLocalModule moduleLocalModules
  where
    moduleFunctions = [f | StatementFunction f <- body ^. moduleStatements]
    moduleLocalModules = [f | StatementLocalModule f <- body ^. moduleStatements]

scanLocalModule :: Members '[State CallMap, Reader InfoTable] r => LocalModule -> Sem r ()
scanLocalModule m = scanModuleBody (m ^. moduleBody)

scanFunctionDef ::
  Members '[State CallMap, Reader InfoTable] r =>
  FunctionDef ->
  Sem r ()
scanFunctionDef FunctionDef {..} =
  runReader (FunctionRef _funDefName) $ do
    scanTypeSignature _funDefTypeSig
    mapM_ scanFunctionClause _funDefClauses

scanTypeSignature ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable] r =>
  Expression ->
  Sem r ()
scanTypeSignature = runReader emptySizeInfo . scanExpression

scanFunctionClause ::
  forall r.
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable] r =>
  FunctionClause ->
  Sem r ()
scanFunctionClause FunctionClause {..} = go (reverse _clausePatterns) _clauseBody
  where
    go :: [PatternArg] -> Expression -> Sem r ()
    go revArgs body = case body of
      ExpressionLambda (Lambda cl) -> mapM_ goClause cl
      _ -> runReader (mkSizeInfo (reverse revArgs)) (scanExpression body)
      where
        goClause :: LambdaClause -> Sem r ()
        goClause (LambdaClause pats clBody) = go (reverse (toList pats) ++ revArgs) clBody

scanExpression ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  Expression ->
  Sem r ()
scanExpression e =
  viewCall e >>= \case
    Just c -> do
      registerCall c
      mapM_ (scanExpression . snd) (c ^. callArgs)
    Nothing -> case e of
      ExpressionApplication a -> scanApplication a
      ExpressionFunction f -> scanFunction f
      ExpressionLambda l -> scanLambda l
      ExpressionIden {} -> return ()
      ExpressionHole {} -> return ()
      ExpressionUniverse {} -> return ()
      ExpressionLiteral {} -> return ()

scanLambda ::
  forall r.
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  Lambda ->
  Sem r ()
scanLambda (Lambda cl) = mapM_ scanClause cl
  where
    scanClause :: LambdaClause -> Sem r ()
    scanClause LambdaClause {..} = scanExpression _lambdaBody

scanApplication ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  Application ->
  Sem r ()
scanApplication (Application l r _) = do
  scanExpression l
  scanExpression r

scanFunction ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  Function ->
  Sem r ()
scanFunction (Function l r) = do
  scanFunctionParameter l
  scanExpression r

scanFunctionParameter ::
  Members '[State CallMap, Reader FunctionRef, Reader InfoTable, Reader SizeInfo] r =>
  FunctionParameter ->
  Sem r ()
scanFunctionParameter p = scanExpression (p ^. paramType)
