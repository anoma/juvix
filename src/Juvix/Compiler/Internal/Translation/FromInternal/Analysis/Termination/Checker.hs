module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Error,
  )
where

import Data.HashMap.Internal.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InfoTable as Internal
import Juvix.Compiler.Internal.Language as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.LexOrder
import Juvix.Prelude

checkTermination ::
  Members '[Error TerminationError] r =>
  InfoTable ->
  Module ->
  Sem r ()
checkTermination infotable topModule = do
  let callmap = buildCallMap infotable topModule
      completeGraph = completeCallGraph callmap
      rEdges = reflexiveEdges completeGraph
      recBehav = map recursiveBehaviour rEdges
  forM_ recBehav $ \r -> do
    let funName = r ^. recursiveBehaviourFun
        markedTerminating :: Bool = funInfo ^. (Internal.functionInfoDef . Internal.funDefTerminating)
        funInfo :: FunctionInfo
        funInfo = HashMap.lookupDefault err funName (infotable ^. Internal.infoFunctions)
          where
            err = error ("Impossible: function not found: " <> funName ^. nameText)
    if
        | markedTerminating -> return ()
        | otherwise ->
            case findOrder r of
              Nothing -> throw (ErrNoLexOrder (NoLexOrder funName))
              Just _ -> return ()

buildCallMap :: InfoTable -> Module -> CallMap
buildCallMap infotable = run . execState mempty . runReader infotable . scanModule

scanModule ::
  Members '[State CallMap] r =>
  Module ->
  Sem r ()
scanModule m = scanModuleBody (m ^. moduleBody)

scanModuleBody :: (Members '[State CallMap] r) => ModuleBody -> Sem r ()
scanModuleBody body =
  mapM_ scanFunctionDef moduleFunctions
  where
    moduleFunctions =
      [ f | StatementMutual (MutualBlock m) <- body ^. moduleStatements, StatementFunction f <- toList m
      ]

scanFunctionDef ::
  (Members '[State CallMap] r) =>
  FunctionDef ->
  Sem r ()
scanFunctionDef FunctionDef {..} =
  runReader _funDefName $ do
    scanTypeSignature _funDefType
    mapM_ scanFunctionClause _funDefClauses

scanTypeSignature ::
  (Members '[State CallMap, Reader FunctionRef] r) =>
  Expression ->
  Sem r ()
scanTypeSignature = runReader emptySizeInfo . scanExpression

scanFunctionClause ::
  forall r.
  (Members '[State CallMap, Reader FunctionRef] r) =>
  FunctionClause ->
  Sem r ()
scanFunctionClause FunctionClause {..} = go (reverse _clausePatterns) _clauseBody
  where
    go :: [PatternArg] -> Expression -> Sem r ()
    go revArgs body = case body of
      ExpressionLambda Lambda {..} -> mapM_ goClause _lambdaClauses
      _ -> runReader (mkSizeInfo (reverse revArgs)) (scanExpression body)
      where
        goClause :: LambdaClause -> Sem r ()
        goClause (LambdaClause pats clBody) = go (reverse (toList pats) ++ revArgs) clBody

scanCase ::
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
  Case ->
  Sem r ()
scanCase c = do
  mapM_ scanCaseBranch (c ^. caseBranches)
  scanExpression (c ^. caseExpression)

scanCaseBranch ::
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
  CaseBranch ->
  Sem r ()
scanCaseBranch = scanExpression . (^. caseBranchExpression)

scanLet ::
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
  Let ->
  Sem r ()
scanLet l = do
  mapM_ scanLetClause (l ^. letClauses)
  scanExpression (l ^. letExpression)

-- NOTE that we forget about the arguments of the hosting function
scanLetClause :: Members '[State CallMap] r => LetClause -> Sem r ()
scanLetClause = \case
  LetFunDef d -> scanFunctionDef d
  LetMutualBlock m -> scanMutualBlockLet m

scanMutualBlockLet :: Members '[State CallMap] r => MutualBlockLet -> Sem r ()
scanMutualBlockLet MutualBlockLet {..} = mapM_ scanFunctionDef _mutualLet

scanExpression ::
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
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
      ExpressionLet l -> scanLet l
      ExpressionCase l -> scanCase l
      ExpressionSimpleLambda l -> scanSimpleLambda l
      ExpressionIden {} -> return ()
      ExpressionHole {} -> return ()
      ExpressionUniverse {} -> return ()
      ExpressionLiteral {} -> return ()

scanSimpleLambda ::
  forall r.
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
  SimpleLambda ->
  Sem r ()
scanSimpleLambda SimpleLambda {..} = scanExpression _slambdaBody

scanLambda ::
  forall r.
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
  Lambda ->
  Sem r ()
scanLambda Lambda {..} = mapM_ scanClause _lambdaClauses
  where
    scanClause :: LambdaClause -> Sem r ()
    scanClause LambdaClause {..} = scanExpression _lambdaBody

scanApplication ::
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
  Application ->
  Sem r ()
scanApplication (Application l r _) = do
  scanExpression l
  scanExpression r

scanFunction ::
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
  Function ->
  Sem r ()
scanFunction (Function l r) = do
  scanFunctionParameter l
  scanExpression r

scanFunctionParameter ::
  (Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r) =>
  FunctionParameter ->
  Sem r ()
scanFunctionParameter p = scanExpression (p ^. paramType)
