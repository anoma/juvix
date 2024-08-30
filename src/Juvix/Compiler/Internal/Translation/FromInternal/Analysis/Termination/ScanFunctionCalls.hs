module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.ScanFunctionCalls where

import Juvix.Compiler.Internal.Extra.Base (directExpressions_)
import Juvix.Compiler.Internal.Language as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Prelude

type CallMap = CallMap' Expression

class Scannable a where
  buildCallMap :: a -> CallMap

instance Scannable Module where
  buildCallMap =
    run
      . execState emptyCallMap
      . scanModule

instance Scannable Expression where
  buildCallMap =
    run
      . execState emptyCallMap
      . scanTopExpression

scanModule ::
  (Members '[State (CallMap' Expression)] r) =>
  Module ->
  Sem r ()
scanModule m = scanModuleBody (m ^. moduleBody)

scanModuleBody :: (Members '[State CallMap] r) => ModuleBody -> Sem r ()
scanModuleBody body = mapM_ scanMutual (body ^. moduleStatements)

scanMutual :: (Members '[State CallMap] r) => MutualBlock -> Sem r ()
scanMutual (MutualBlock ss) = mapM_ scanMutualStatement ss

scanInductive :: forall r. (Members '[State CallMap] r) => InductiveDef -> Sem r ()
scanInductive i = do
  scanTopExpression (i ^. inductiveType)
  mapM_ scanConstructor (i ^. inductiveConstructors)
  where
    scanConstructor :: ConstructorDef -> Sem r ()
    scanConstructor c = scanTopExpression (c ^. inductiveConstructorType)

scanMutualStatement :: (Members '[State CallMap] r) => MutualStatement -> Sem r ()
scanMutualStatement = \case
  StatementInductive i -> scanInductive i
  StatementFunction i -> scanFunctionDef i
  StatementAxiom a -> scanAxiom a

scanAxiom :: (Members '[State CallMap] r) => AxiomDef -> Sem r ()
scanAxiom = scanTopExpression . (^. axiomType)

scanFunctionDef ::
  (Members '[State CallMap] r) =>
  FunctionDef ->
  Sem r ()
scanFunctionDef f@FunctionDef {..} = do
  registerFunctionDef @Expression Proxy f
  runReader (Just _funDefName) $ do
    scanTypeSignature _funDefType
    scanFunctionBody _funDefBody
    scanDefaultArgs _funDefArgsInfo

scanDefaultArgs ::
  forall r.
  (Members '[Reader (Maybe FunctionName), State CallMap] r) =>
  [ArgInfo] ->
  Sem r ()
scanDefaultArgs = mapM_ scanArgInfo
  where
    scanArgInfo :: ArgInfo -> Sem r ()
    scanArgInfo = mapM_ scanTypeSignature . (^. argInfoDefault)

scanTypeSignature ::
  (Members '[State CallMap, Reader (Maybe FunctionName)] r) =>
  Expression ->
  Sem r ()
scanTypeSignature = runReader emptySizeInfo . scanExpression

scanFunctionBody ::
  forall r.
  (Members '[State CallMap, Reader (Maybe FunctionName)] r) =>
  Expression ->
  Sem r ()
scanFunctionBody topbody = go [] topbody
  where
    go :: [PatternArg] -> Expression -> Sem r ()
    go revArgs body = case body of
      ExpressionLambda Lambda {..} -> mapM_ goClause _lambdaClauses
      _ -> runReader (mkSizeInfo (reverse revArgs)) (scanExpression body)
      where
        goClause :: LambdaClause -> Sem r ()
        goClause (LambdaClause pats clBody) = go (reverse (toList pats) ++ revArgs) clBody

scanLet ::
  (Members '[State CallMap, Reader (Maybe FunctionName), Reader SizeInfo] r) =>
  Let ->
  Sem r ()
scanLet l = do
  mapM_ scanLetClause (l ^. letClauses)
  scanExpression (l ^. letExpression)

-- NOTE that we forget about the arguments of the hosting function
scanLetClause :: (Members '[State CallMap] r) => LetClause -> Sem r ()
scanLetClause = \case
  LetFunDef d -> scanFunctionDef d
  LetMutualBlock m -> scanMutualBlockLet m

scanMutualBlockLet :: (Members '[State CallMap] r) => MutualBlockLet -> Sem r ()
scanMutualBlockLet MutualBlockLet {..} = mapM_ scanFunctionDef _mutualLet

scanTopExpression ::
  (Members '[State CallMap] r) =>
  Expression ->
  Sem r ()
scanTopExpression =
  runReader (Nothing @FunctionName)
    . runReader emptySizeInfo
    . scanExpression

scanExpression ::
  (Members '[State CallMap, Reader (Maybe FunctionName), Reader SizeInfo] r) =>
  Expression ->
  Sem r ()
scanExpression e =
  viewCall e >>= \case
    Just c -> do
      whenJustM (ask @(Maybe FunctionName)) (\caller -> runReader caller (registerCall c))
      mapM_ (scanExpression . (^. argExpression)) (c ^. callArgs)
    Nothing -> case e of
      ExpressionApplication a -> directExpressions_ scanExpression a
      ExpressionFunction f -> directExpressions_ scanExpression f
      ExpressionLambda l -> directExpressions_ scanExpression l
      ExpressionLet l -> scanLet l
      ExpressionCase l -> directExpressions_ scanExpression l
      ExpressionSimpleLambda l -> directExpressions_ scanExpression l
      ExpressionIden {} -> return ()
      ExpressionHole {} -> return ()
      ExpressionInstanceHole {} -> return ()
      ExpressionUniverse {} -> return ()
      ExpressionLiteral {} -> return ()
