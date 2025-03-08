module Juvix.Compiler.Internal.Extra.DependencyBuilder
  ( NameDependencyInfo,
    buildDependencyInfoPreModule,
    buildDependencyInfoLet,
    instanceDependencyParams,
    letDependencyParams,
    positivityNameDependencyInfo,
    DependencyParams (..),
    dependencyParamsIsStartNode,
    dependencyParamsInstance,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.NameDependencyInfo
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

-- adjacency set representation
type DependencyGraph = HashMap Name (HashSet Name)

type StartNodes = HashSet Name

data BuilderState = BuilderState
  { _builderStateNat :: Maybe Name,
    _builderStateFromNat :: Maybe Name,
    _builderStateInt :: Maybe Name,
    _builderStateFromInt :: Maybe Name
  }

makeLenses ''BuilderState

emptyBuilderState :: BuilderState
emptyBuilderState =
  BuilderState
    { _builderStateNat = Nothing,
      _builderStateFromNat = Nothing,
      _builderStateInt = Nothing,
      _builderStateFromInt = Nothing
    }

letDependencyParams :: DependencyParams
letDependencyParams =
  DependencyParams
    { _dependencyParamsIsStartNode = const False,
      _dependencyParamsInstance = False
    }

instanceDependencyParams :: HashSet NameId -> DependencyParams
instanceDependencyParams s =
  DependencyParams
    { _dependencyParamsIsStartNode = (`HashSet.member` s),
      _dependencyParamsInstance = True
    }

data DependencyParams = DependencyParams
  { _dependencyParamsIsStartNode :: NameId -> Bool,
    -- | When set to True, each declaration depends on the previous declaration.
    -- Necessary for instance resolution
    _dependencyParamsInstance :: Bool
  }

makeLenses ''DependencyParams

buildDependencyInfoPreModule :: forall r. (Members '[Reader DependencyParams] r) => PreModule -> Sem r NameDependencyInfo
buildDependencyInfoPreModule ms =
  buildDependencyInfoHelper (goPreModule ms >> addCastEdges)

-- | Compute dependency info with `_dependencyParamsInstance` set to `False`.
-- Used for positivity checking
positivityNameDependencyInfo :: [PreStatement] -> NameDependencyInfo
positivityNameDependencyInfo m =
  run
    . runReader dependencyParams
    . buildDependencyInfoHelper
    $ goPreStatements impossibleParent m
  where
    impossibleParent :: Name
    impossibleParent = impossibleError "This name should never be used because `_dependencyParamsInstance` is set to False"

    dependencyParams :: DependencyParams
    dependencyParams =
      DependencyParams
        { _dependencyParamsInstance = False,
          _dependencyParamsIsStartNode = const True
        }

buildDependencyInfoLet :: NonEmpty PreLetStatement -> NameDependencyInfo
buildDependencyInfoLet ls =
  run . runReader letDependencyParams $
    buildDependencyInfoHelper (goPreLetStatements Nothing (toList ls) >> addCastEdges)

buildDependencyInfoHelper ::
  Sem (State DependencyGraph ': State StartNodes ': State BuilderState ': r) () ->
  Sem r NameDependencyInfo
buildDependencyInfoHelper m = do
  (startNodes :: StartNodes, graph :: DependencyGraph) <-
    evalState emptyBuilderState
      . runState HashSet.empty
      . execState HashMap.empty
      $ m
  return (createDependencyInfo graph startNodes)

addCastEdges :: (Members '[State DependencyGraph, State BuilderState] r) => Sem r ()
addCastEdges = do
  nat <- gets (^. builderStateNat)
  fromNat <- gets (^. builderStateFromNat)
  case (nat, fromNat) of
    (Just nat', Just fromNat') -> addEdge nat' fromNat'
    _ -> return ()
  int <- gets (^. builderStateInt)
  fromInt <- gets (^. builderStateFromInt)
  case (int, fromInt) of
    (Just int', Just fromInt') -> addEdge int' fromInt'
    _ -> return ()

addStartNode :: (Member (State StartNodes) r) => Name -> Sem r ()
addStartNode n = modify (HashSet.insert n)

addEdgeParent :: (Members '[Reader DependencyParams, State DependencyGraph] r) => Name -> Name -> Sem r ()
addEdgeParent a b = do
  inst <- asks (^. dependencyParamsInstance)
  when inst (addEdge a b)

addEdgeMay :: (Members '[State DependencyGraph, Reader (Maybe Name)] r) => Name -> Sem r ()
addEdgeMay n2 = whenJustM ask $ \n1 -> addEdge n1 n2

addEdgeMayRev :: (Members '[State DependencyGraph, Reader (Maybe Name)] r) => Name -> Sem r ()
addEdgeMayRev n2 = whenJustM ask $ \n1 -> addEdge n2 n1

addNode :: (Member (State DependencyGraph) r) => Name -> Sem r ()
addNode n =
  modify @DependencyGraph . over (at n) $
    \case
      Just x -> Just x
      Nothing -> Just (mempty :: HashSet Name)

addEdge :: (Member (State DependencyGraph) r) => Name -> Name -> Sem r ()
addEdge n1 n2 =
  modify @DependencyGraph . over (at n1) $ \case
    Just ns -> Just (HashSet.insert n2 ns)
    Nothing -> Just (HashSet.singleton n2)

checkStartNode :: (Members '[Reader DependencyParams, State StartNodes, State BuilderState] r) => Name -> Sem r ()
checkStartNode n = do
  isStart <- asks (^. dependencyParamsIsStartNode)
  when (isStart (n ^. nameId)) (addStartNode n)

goPreModule :: (Members '[Reader DependencyParams, State DependencyGraph, State StartNodes, State BuilderState] r) => PreModule -> Sem r ()
goPreModule m = do
  checkStartNode (m ^. moduleName)
  let b = m ^. moduleBody
  -- Declarations in a module depend on the module, not the other way round (a
  -- module is reachable if at least one of the declarations in it is reachable)
  goPreStatements (m ^. moduleName) (b ^. moduleStatements)

goPreLetStatements ::
  forall r.
  (Members '[Reader DependencyParams, State DependencyGraph, State StartNodes, State BuilderState] r) =>
  Maybe Name ->
  [PreLetStatement] ->
  Sem r ()
goPreLetStatements mp = \case
  stmt : stmts -> do
    runReader mp (goPreLetStatement stmt)
    goPreLetStatements (Just (getPreLetStatementName stmt)) stmts
  [] -> return ()
  where
    getPreLetStatementName :: PreLetStatement -> Name
    getPreLetStatementName = \case
      PreLetFunctionDef f -> f ^. funDefName

goPreLetStatement ::
  forall r.
  (Members '[Reader DependencyParams, State DependencyGraph, State StartNodes, State BuilderState, Reader (Maybe Name)] r) =>
  PreLetStatement ->
  Sem r ()
goPreLetStatement = \case
  PreLetFunctionDef f -> do
    addEdgeMayRev (f ^. funDefName)
    goFunctionDefHelper f

-- | `p` is the parent -- the previous declaration or the enclosing module. A
-- declaraction depends on its parent (on the previous declaration in the module
-- if it exists) in order to guarantee that instance declarations are always
-- processed before their uses. For an instance to be taken into account in
-- instance resolution, it needs to be declared textually earlier.
goPreStatements :: forall r. (Members '[Reader DependencyParams, State DependencyGraph, State StartNodes, State BuilderState] r) => Name -> [PreStatement] -> Sem r ()
goPreStatements p = \case
  stmt : stmts -> do
    goPreStatement p stmt
    goPreStatements (getPreStatementName stmt) stmts
  [] -> return ()
  where
    getPreStatementName :: PreStatement -> Name
    getPreStatementName = \case
      PreAxiomDef ax -> ax ^. axiomName
      PreFunctionDef f -> f ^. funDefName
      PreInductiveDef i -> i ^. inductiveName

-- | `p` is the parent -- the previous declaration or the enclosing module
goPreStatement :: forall r. (Members '[Reader DependencyParams, State DependencyGraph, State StartNodes, State BuilderState] r) => Name -> PreStatement -> Sem r ()
goPreStatement p = \case
  PreAxiomDef ax -> goAxiom p ax
  PreFunctionDef f -> goTopFunctionDef p f
  PreInductiveDef i -> goInductive p i

goAxiom :: forall r. (Members '[Reader DependencyParams, State DependencyGraph, State StartNodes, State BuilderState] r) => Name -> AxiomDef -> Sem r ()
goAxiom p ax = do
  checkStartNode (ax ^. axiomName)
  addEdgeParent (ax ^. axiomName) p
  runReader (Just (ax ^. axiomName)) (goExpression (ax ^. axiomType))

goInductive :: forall r. (Members '[Reader DependencyParams, State DependencyGraph, State StartNodes, State BuilderState] r) => Name -> InductiveDef -> Sem r ()
goInductive p i = do
  let indName = i ^. inductiveName
  checkStartNode indName
  checkBuiltinInductiveStartNode i
  addEdgeParent indName p
  mapM_ (goConstructorDef indName) (i ^. inductiveConstructors)
  runReader (Just indName) $ do
    mapM_ goInductiveParameter (i ^. inductiveParameters)
    goExpression (i ^. inductiveType)

-- | BuiltinBool and BuiltinNat are required by the Internal to Core translation
-- when translating literal integers to Nats.
checkBuiltinInductiveStartNode :: forall r. (Members '[State StartNodes, State BuilderState] r) => InductiveDef -> Sem r ()
checkBuiltinInductiveStartNode i = whenJust (i ^. inductiveBuiltin) go
  where
    go :: BuiltinInductive -> Sem r ()
    go = \case
      BuiltinNat -> do
        modify (set builderStateNat (Just (i ^. inductiveName)))
        addInductiveStartNode
      BuiltinBool -> addInductiveStartNode
      BuiltinInt -> do
        modify (set builderStateInt (Just (i ^. inductiveName)))
        addInductiveStartNode
      BuiltinList -> return ()
      BuiltinMaybe -> return ()
      BuiltinPair -> return ()
      BuiltinJson -> return ()
      BuiltinPoseidonState -> return ()
      BuiltinEcPoint -> return ()
      BuiltinAnomaResource -> return ()
      BuiltinAnomaAction -> return ()
      BuiltinAnomaComplianceInputs -> return ()
      BuiltinAnomaShieldedTransaction -> return ()
      BuiltinEq -> return ()
      BuiltinOrd -> return ()
      BuiltinOrdering -> return ()

    addInductiveStartNode :: Sem r ()
    addInductiveStartNode = addStartNode (i ^. inductiveName)

goTopFunctionDef :: (Members '[State DependencyGraph, State StartNodes, State BuilderState, Reader DependencyParams] r) => Name -> FunctionDef -> Sem r ()
goTopFunctionDef p f = do
  addEdgeParent (f ^. funDefName) p
  goFunctionDefHelper f

checkCast ::
  (Member (State BuilderState) r) =>
  FunctionDef ->
  Sem r ()
checkCast f = case f ^. funDefBuiltin of
  Just BuiltinFromNat -> modify (set builderStateFromNat (Just (f ^. funDefName)))
  Just BuiltinFromInt -> modify (set builderStateFromInt (Just (f ^. funDefName)))
  _ -> return ()

goFunctionDefHelper ::
  (Members '[State DependencyGraph, State StartNodes, State BuilderState, Reader DependencyParams] r) =>
  FunctionDef ->
  Sem r ()
goFunctionDefHelper f = do
  addNode (f ^. funDefName)
  checkStartNode (f ^. funDefName)
  checkCast f
  runReader (Just (f ^. funDefName)) $ do
    goExpression (f ^. funDefType)
    goExpression (f ^. funDefBody)
    mapM_ goExpression (f ^.. funDefArgsInfo . each . argInfoDefault . _Just)

-- | constructors of an inductive type depend on the inductive type, not the other
-- way round; an inductive type depends on the types of its constructors
goConstructorDef :: (Members '[State DependencyGraph, State StartNodes, State BuilderState, Reader DependencyParams] r) => Name -> ConstructorDef -> Sem r ()
goConstructorDef indName c = do
  addEdge (c ^. inductiveConstructorName) indName
  runReader (Just indName) (goExpression (c ^. inductiveConstructorType))

goPattern :: forall r. (Members '[State DependencyGraph, Reader (Maybe Name)] r) => PatternArg -> Sem r ()
goPattern p = case p ^. patternArgPattern of
  PatternVariable {} -> return ()
  PatternWildcardConstructor c -> goWildcardConstructor c
  PatternConstructorApp a -> goApp a
  where
    goWildcardConstructor :: WildcardConstructor -> Sem r ()
    goWildcardConstructor w = addEdgeMay (w ^. wildcardConstructor)

    goApp :: ConstructorApp -> Sem r ()
    goApp (ConstructorApp ctr ps _) = do
      addEdgeMay ctr
      mapM_ goPattern ps

goExpression ::
  forall r.
  (Members '[State DependencyGraph, State StartNodes, State BuilderState, Reader DependencyParams, Reader (Maybe Name)] r) =>
  Expression ->
  Sem r ()
goExpression e = case e of
  ExpressionIden i -> addEdgeMay (i ^. idenName)
  ExpressionUniverse {} -> return ()
  ExpressionFunction f -> do
    goFunctionParameter (f ^. functionLeft)
    goExpression (f ^. functionRight)
  ExpressionApplication (Application l r _) -> do
    goExpression l
    goExpression r
  ExpressionLiteral {} -> return ()
  ExpressionCase c -> goCase c
  ExpressionHole {} -> return ()
  ExpressionInstanceHole {} -> return ()
  ExpressionLambda l -> goLambda l
  ExpressionLet l -> goLet l
  ExpressionSimpleLambda l -> goSimpleLambda l
  where
    goSimpleLambda :: SimpleLambda -> Sem r ()
    goSimpleLambda l = do
      addEdgeMay (l ^. slambdaBinder . sbinderVar)

    goLambda :: Lambda -> Sem r ()
    goLambda Lambda {..} = mapM_ goClause _lambdaClauses
      where
        goClause :: LambdaClause -> Sem r ()
        goClause LambdaClause {..} = do
          goExpression _lambdaBody
          mapM_ goPattern _lambdaPatterns

    goCase :: Case -> Sem r ()
    goCase c = do
      goExpression (c ^. caseExpression)
      mapM_ goBranch (c ^. caseBranches)

    goBranch :: CaseBranch -> Sem r ()
    goBranch c = do
      goPattern (c ^. caseBranchPattern)
      goCaseBranchRhs (c ^. caseBranchRhs)

    goSideIfBranch :: SideIfBranch -> Sem r ()
    goSideIfBranch SideIfBranch {..} = do
      goExpression _sideIfBranchCondition
      goExpression _sideIfBranchBody

    goSideIfs :: SideIfs -> Sem r ()
    goSideIfs SideIfs {..} = do
      mapM_ goSideIfBranch _sideIfBranches
      mapM_ goExpression _sideIfElse

    goCaseBranchRhs :: CaseBranchRhs -> Sem r ()
    goCaseBranchRhs = \case
      CaseBranchRhsExpression expr -> goExpression expr
      CaseBranchRhsIf s -> goSideIfs s

    goLet :: Let -> Sem r ()
    goLet l = do
      mapM_ goLetClause (l ^. letClauses)
      goExpression (l ^. letExpression)

    goLetClause :: LetClause -> Sem r ()
    goLetClause = \case
      LetFunDef f -> do
        addEdgeMay (f ^. funDefName)
        goFunctionDefHelper f
      LetMutualBlock MutualBlockLet {..} -> mapM_ goFunctionDefHelper _mutualLet

goInductiveParameter ::
  (Members '[State DependencyGraph, State StartNodes, State BuilderState, Reader DependencyParams, Reader (Maybe Name)] r) =>
  InductiveParameter ->
  Sem r ()
goInductiveParameter param = do
  addEdgeMay (param ^. inductiveParamName)
  goExpression (param ^. inductiveParamType)

goFunctionParameter ::
  (Members '[State DependencyGraph, State StartNodes, State BuilderState, Reader DependencyParams, Reader (Maybe Name)] r) =>
  FunctionParameter ->
  Sem r ()
goFunctionParameter param = do
  whenJust (param ^. paramName) addEdgeMay
  goExpression (param ^. paramType)
