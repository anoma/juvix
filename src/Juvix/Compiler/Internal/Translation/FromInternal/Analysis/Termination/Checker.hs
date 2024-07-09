module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
  ( Termination,
    buildCallMap,
    checkTerminationShallow,
    runTermination,
    evalTermination,
    execTermination,
    functionSafeToNormalize,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.TerminationState,
  )
where

import Data.HashMap.Internal.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra.Base (directExpressions_)
import Juvix.Compiler.Internal.Language as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.TerminationState
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.LexOrder
import Juvix.Prelude

class Scannable a where
  buildCallMap :: a -> CallMap

data Termination :: Effect where
  CheckTerminationShallow :: (Scannable a) => a -> Termination m ()
  FunctionTermination :: FunctionRef -> Termination m IsTerminating

makeSem ''Termination

functionSafeToNormalize :: (Members '[Termination] r) => FunctionRef -> Sem r Bool
functionSafeToNormalize = fmap safeToNormalize . functionTermination

runTermination :: forall r a. (Members '[Error JuvixError] r) => TerminationState -> Sem (Termination ': r) a -> Sem r (TerminationState, a)
runTermination ini m = do
  res <- runTerminationState ini m
  checkNonTerminating (fst res)
  return res
  where
    checkNonTerminating :: TerminationState -> Sem r ()
    checkNonTerminating i =
      whenJust (i ^. terminationFailedSet . to (nonEmpty . toList)) $
        throw . JuvixError . ErrNoLexOrder . NoLexOrder

evalTermination :: (Members '[Error JuvixError] r) => TerminationState -> Sem (Termination ': r) a -> Sem r a
evalTermination s = fmap snd . runTermination s

execTermination :: (Members '[Error JuvixError] r) => TerminationState -> Sem (Termination ': r) a -> Sem r TerminationState
execTermination s = fmap fst . runTermination s

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

runTerminationState :: TerminationState -> Sem (Termination ': r) a -> Sem r (TerminationState, a)
runTerminationState ini = reinterpret (runState ini) $ \case
  CheckTerminationShallow m -> checkTerminationShallow' m
  FunctionTermination m -> functionTermination' m

-- | If the function is missing, can we assume that it is not recursive
functionTermination' ::
  forall r.
  (Members '[State TerminationState] r) =>
  FunctionName ->
  Sem r IsTerminating
functionTermination' f = fromMaybe TerminatingChecked <$> gets (^. terminationTable . at f)

-- | Returns the set of non-terminating functions. Does not go into imports.
checkTerminationShallow' ::
  forall r m.
  (Members '[State TerminationState] r, Scannable m) =>
  m ->
  Sem r ()
checkTerminationShallow' topModule = do
  let callmap = buildCallMap topModule
      completeGraph = completeCallGraph callmap
      rEdges = reflexiveEdges completeGraph
      recBehav = map recursiveBehaviour rEdges
  forM_ recBehav $ \rb -> do
    let funName = rb ^. recursiveBehaviourFun
        markedTerminating :: Bool = funInfo ^. Internal.funDefTerminating
        funInfo :: FunctionDef
        funInfo = HashMap.lookupDefault err funName (callmap ^. callMapScanned)
          where
            err = error ("Impossible: function not found: " <> funName ^. nameText)
        order = findOrder rb
    addTerminating funName $
      if
          | Just {} <- order -> TerminatingChecked
          | markedTerminating -> TerminatingFailedMarked
          | Nothing <- order -> TerminatingFailed

scanModule ::
  (Members '[State CallMap] r) =>
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
  registerFunctionDef f
  runReader (Just _funDefName) $ do
    scanTypeSignature _funDefType
    scanFunctionBody _funDefBody
    scanDefaultArgs _funDefArgsInfo

scanDefaultArgs ::
  forall r.
  (Members '[Reader (Maybe FunctionRef), State CallMap] r) =>
  [ArgInfo] ->
  Sem r ()
scanDefaultArgs = mapM_ scanArgInfo
  where
    scanArgInfo :: ArgInfo -> Sem r ()
    scanArgInfo = mapM_ scanTypeSignature . (^. argInfoDefault)

scanTypeSignature ::
  (Members '[State CallMap, Reader (Maybe FunctionRef)] r) =>
  Expression ->
  Sem r ()
scanTypeSignature = runReader emptySizeInfo . scanExpression

scanFunctionBody ::
  forall r.
  (Members '[State CallMap, Reader (Maybe FunctionRef)] r) =>
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
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
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
  runReader (Nothing @FunctionRef)
    . runReader emptySizeInfo
    . scanExpression

scanExpression ::
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
  Expression ->
  Sem r ()
scanExpression e =
  viewCall e >>= \case
    Just c -> do
      whenJustM (ask @(Maybe FunctionRef)) (\caller -> runReader caller (registerCall c))
      mapM_ (scanExpression . snd) (c ^. callArgs)
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
