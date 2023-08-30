module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
  ( Termination,
    buildCallMap,
    checkTerminationShallow,
    runTermination,
    evalTermination,
    execTermination,
    functionIsTerminating,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.TerminationState,
  )
where

import Data.HashMap.Internal.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.TerminationState
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.LexOrder
import Juvix.Prelude

class Scannable a where
  buildCallMap :: a -> CallMap

data Termination m a where
  CheckTerminationShallow :: (Scannable a) => a -> Termination m ()
  FunctionTermination :: FunctionRef -> Termination m IsTerminating

makeSem ''Termination

functionIsTerminating :: (Members '[Termination] r) => FunctionRef -> Sem r Bool
functionIsTerminating = fmap terminates . functionTermination
  where
    terminates :: IsTerminating -> Bool
    terminates = \case
      TerminatingCheckedOrMarked -> True
      TerminatingFailed -> False

runTermination :: forall r a. (Members '[Error JuvixError] r) => TerminationState -> Sem (Termination ': r) a -> Sem r (TerminationState, a)
runTermination ini m = do
  res <- runState ini (re m)
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

instance Scannable Import where
  buildCallMap = buildCallMap . (^. importModule . moduleIxModule)

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

re :: Sem (Termination ': r) a -> Sem (State TerminationState ': r) a
re = reinterpret $ \case
  CheckTerminationShallow m -> checkTerminationShallow' m
  FunctionTermination m -> functionTermination' m

-- | If the function is missing, can we assume that it is not recursive
functionTermination' ::
  forall r.
  (Members '[State TerminationState] r) =>
  FunctionName ->
  Sem r IsTerminating
functionTermination' f = fromMaybe TerminatingCheckedOrMarked <$> gets (^. terminationTable . at f)

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
          | markedTerminating -> TerminatingCheckedOrMarked
          | Nothing <- order -> TerminatingFailed
          | Just {} <- order -> TerminatingCheckedOrMarked

scanModule ::
  (Members '[State CallMap] r) =>
  Module ->
  Sem r ()
scanModule m = scanModuleBody (m ^. moduleBody)

scanModuleBody :: (Members '[State CallMap] r) => ModuleBody -> Sem r ()
scanModuleBody body = mapM_ scanStatement (body ^. moduleStatements)

scanStatement :: (Members '[State CallMap] r) => Statement -> Sem r ()
scanStatement = \case
  StatementAxiom a -> scanAxiom a
  StatementMutual m -> scanMutual m

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
    mapM_ scanFunctionClause _funDefClauses

scanTypeSignature ::
  (Members '[State CallMap, Reader (Maybe FunctionRef)] r) =>
  Expression ->
  Sem r ()
scanTypeSignature = runReader emptySizeInfo . scanExpression

scanFunctionClause ::
  forall r.
  (Members '[State CallMap, Reader (Maybe FunctionRef)] r) =>
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
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
  Case ->
  Sem r ()
scanCase c = do
  mapM_ scanCaseBranch (c ^. caseBranches)
  scanExpression (c ^. caseExpression)

scanCaseBranch ::
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
  CaseBranch ->
  Sem r ()
scanCaseBranch = scanExpression . (^. caseBranchExpression)

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
scanTopExpression = runReader (Nothing @FunctionRef) . runReader emptySizeInfo . scanExpression

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
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
  SimpleLambda ->
  Sem r ()
scanSimpleLambda SimpleLambda {..} = scanExpression _slambdaBody

scanLambda ::
  forall r.
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
  Lambda ->
  Sem r ()
scanLambda Lambda {..} = mapM_ scanClause _lambdaClauses
  where
    scanClause :: LambdaClause -> Sem r ()
    scanClause LambdaClause {..} = scanExpression _lambdaBody

scanApplication ::
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
  Application ->
  Sem r ()
scanApplication (Application l r _) = do
  scanExpression l
  scanExpression r

scanFunction ::
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
  Function ->
  Sem r ()
scanFunction (Function l r) = do
  scanFunctionParameter l
  scanExpression r

scanFunctionParameter ::
  (Members '[State CallMap, Reader (Maybe FunctionRef), Reader SizeInfo] r) =>
  FunctionParameter ->
  Sem r ()
scanFunctionParameter p = scanExpression (p ^. paramType)
