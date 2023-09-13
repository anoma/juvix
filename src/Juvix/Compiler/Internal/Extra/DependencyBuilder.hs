module Juvix.Compiler.Internal.Extra.DependencyBuilder
  ( buildDependencyInfo,
    buildDependencyInfoPreModule,
    buildDependencyInfoExpr,
    buildDependencyInfoLet,
    ExportsTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.NameDependencyInfo
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

-- adjacency set representation
type DependencyGraph = HashMap Name (HashSet Name)

type StartNodes = HashSet Name

type ExportsTable = HashSet NameId

buildDependencyInfoPreModule :: PreModule -> ExportsTable -> NameDependencyInfo
buildDependencyInfoPreModule ms tab =
  buildDependencyInfoHelper tab (goPreModule ms)

buildDependencyInfo :: NonEmpty Module -> ExportsTable -> NameDependencyInfo
buildDependencyInfo ms tab =
  buildDependencyInfoHelper tab (mapM_ (visit . ModuleIndex) ms)

buildDependencyInfoExpr :: Expression -> NameDependencyInfo
buildDependencyInfoExpr = buildDependencyInfoHelper mempty . goExpression Nothing

buildDependencyInfoLet :: NonEmpty PreLetStatement -> NameDependencyInfo
buildDependencyInfoLet = buildDependencyInfoHelper mempty . mapM_ goPreLetStatement

buildDependencyInfoHelper ::
  ExportsTable ->
  Sem '[Visit ModuleIndex, Reader ExportsTable, State DependencyGraph, State StartNodes] () ->
  NameDependencyInfo
buildDependencyInfoHelper tbl m = createDependencyInfo graph startNodes
  where
    startNodes :: StartNodes
    graph :: DependencyGraph
    (startNodes, graph) =
      run
        . runState HashSet.empty
        . execState HashMap.empty
        . runReader tbl
        . evalVisitEmpty goModuleNoVisited
        $ m

addStartNode :: (Member (State StartNodes) r) => Name -> Sem r ()
addStartNode n = modify (HashSet.insert n)

addEdgeMay :: (Member (State DependencyGraph) r) => Maybe Name -> Name -> Sem r ()
addEdgeMay mn1 n2 = whenJust mn1 $ \n1 -> addEdge n1 n2

addEdge :: (Member (State DependencyGraph) r) => Name -> Name -> Sem r ()
addEdge n1 n2 =
  modify
    ( HashMap.alter
        ( \case
            Just ns -> Just (HashSet.insert n2 ns)
            Nothing -> Just (HashSet.singleton n2)
        )
        n1
    )

checkStartNode :: (Members '[Reader ExportsTable, State StartNodes] r) => Name -> Sem r ()
checkStartNode n = do
  tab <- ask
  when
    (HashSet.member (n ^. nameId) tab)
    (addStartNode n)

goModuleNoVisited :: forall r. (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, Visit ModuleIndex] r) => ModuleIndex -> Sem r ()
goModuleNoVisited (ModuleIndex m) = do
  checkStartNode (m ^. moduleName)
  let b = m ^. moduleBody
  mapM_ (goStatement (m ^. moduleName)) (b ^. moduleStatements)
  mapM_ goImport (b ^. moduleImports)

goImport :: (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, Visit ModuleIndex] r) => Import -> Sem r ()
goImport (Import m) = visit m

goPreModule :: (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, Visit ModuleIndex] r) => PreModule -> Sem r ()
goPreModule m = do
  checkStartNode (m ^. moduleName)
  let b = m ^. moduleBody
  mapM_ (goPreStatement (m ^. moduleName)) (b ^. moduleStatements)
  -- We cannot ignore imports with instances, because a trait in a module M may
  -- depend on an instance in a module N which imports M  (i.e. new edges may be
  -- added from definitions in M to definitions in N)
  mapM_ goImport (b ^. moduleImports)

goStatement :: forall r. (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes] r) => Name -> Statement -> Sem r ()
goStatement parentModule = \case
  StatementAxiom ax -> goAxiom parentModule ax
  StatementMutual f -> goMutual parentModule f

goMutual :: forall r. (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes] r) => Name -> MutualBlock -> Sem r ()
goMutual parentModule (MutualBlock s) = mapM_ go s
  where
    go :: MutualStatement -> Sem r ()
    go = \case
      StatementInductive i -> goInductive parentModule i
      StatementFunction i -> goTopFunctionDef parentModule i

goPreLetStatement ::
  forall r.
  (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes] r) =>
  PreLetStatement ->
  Sem r ()
goPreLetStatement = \case
  PreLetFunctionDef f -> goFunctionDefHelper f

-- | Declarations in a module depend on the module, not the other way round (a
-- module is reachable if at least one of the declarations in it is reachable)
goPreStatement :: forall r. (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes] r) => Name -> PreStatement -> Sem r ()
goPreStatement parentModule = \case
  PreAxiomDef ax -> goAxiom parentModule ax
  PreFunctionDef f -> goTopFunctionDef parentModule f
  PreInductiveDef i -> goInductive parentModule i

goAxiom :: forall r. (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes] r) => Name -> AxiomDef -> Sem r ()
goAxiom parentModule ax = do
  checkStartNode (ax ^. axiomName)
  addEdge (ax ^. axiomName) parentModule
  goExpression (Just (ax ^. axiomName)) (ax ^. axiomType)

goInductive :: forall r. (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes] r) => Name -> InductiveDef -> Sem r ()
goInductive parentModule i = do
  checkStartNode (i ^. inductiveName)
  checkBuiltinInductiveStartNode i
  addEdge (i ^. inductiveName) parentModule
  mapM_ (goInductiveParameter (Just (i ^. inductiveName))) (i ^. inductiveParameters)
  goExpression (Just (i ^. inductiveName)) (i ^. inductiveType)
  mapM_ (goConstructorDef (i ^. inductiveName)) (i ^. inductiveConstructors)

-- BuiltinBool and BuiltinNat are required by the Internal to Core translation
-- when translating literal integers to Nats.
checkBuiltinInductiveStartNode :: forall r. (Member (State StartNodes) r) => InductiveDef -> Sem r ()
checkBuiltinInductiveStartNode i = whenJust (i ^. inductiveBuiltin) go
  where
    go :: BuiltinInductive -> Sem r ()
    go = \case
      BuiltinNat -> addInductiveStartNode
      BuiltinBool -> addInductiveStartNode
      BuiltinInt -> addInductiveStartNode
      BuiltinList -> return ()

    addInductiveStartNode :: Sem r ()
    addInductiveStartNode = addStartNode (i ^. inductiveName)

goTopFunctionDef :: (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> FunctionDef -> Sem r ()
goTopFunctionDef modName f = do
  addEdge (f ^. funDefName) modName
  goFunctionDefHelper f

-- | An instance must be in the same component as the trait, because before type
-- checking the instance holes are not filled which may result in missing
-- dependencies. In other words, the trait needs to depend on all its instances.
goInstance ::
  (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) =>
  FunctionDef ->
  Sem r ()
goInstance f = do
  let app = snd (unfoldFunType (f ^. funDefType))
      h = fst (unfoldExpressionApp app)
  case h of
    ExpressionIden (IdenInductive i) ->
      addEdge i (f ^. funDefName)
    _ ->
      return ()

goFunctionDefHelper ::
  (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) =>
  FunctionDef ->
  Sem r ()
goFunctionDefHelper f = do
  checkStartNode (f ^. funDefName)
  when (f ^. funDefInstance) $
    goInstance f
  goExpression (Just (f ^. funDefName)) (f ^. funDefType)
  mapM_ (goFunctionClause (f ^. funDefName)) (f ^. funDefClauses)

-- constructors of an inductive type depend on the inductive type, not the other
-- way round; an inductive type depends on the types of its constructors
goConstructorDef :: (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> ConstructorDef -> Sem r ()
goConstructorDef indName c = do
  addEdge (c ^. inductiveConstructorName) indName
  goExpression (Just indName) (c ^. inductiveConstructorType)

goFunctionClause :: (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> FunctionClause -> Sem r ()
goFunctionClause p c = do
  mapM_ (goPattern (Just p)) (c ^. clausePatterns)
  goExpression (Just p) (c ^. clauseBody)

goPattern :: forall r. (Member (State DependencyGraph) r) => Maybe Name -> PatternArg -> Sem r ()
goPattern n p = case p ^. patternArgPattern of
  PatternVariable {} -> return ()
  PatternConstructorApp a -> goApp a
  where
    goApp :: ConstructorApp -> Sem r ()
    goApp (ConstructorApp ctr ps _) = do
      addEdgeMay n ctr
      mapM_ (goPattern n) ps

goExpression ::
  forall r.
  (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) =>
  Maybe Name ->
  Expression ->
  Sem r ()
goExpression p e = case e of
  ExpressionIden i -> addEdgeMay p (idenName i)
  ExpressionUniverse {} -> return ()
  ExpressionFunction f -> do
    goFunctionParameter p (f ^. functionLeft)
    goExpression p (f ^. functionRight)
  ExpressionApplication (Application l r _) -> do
    goExpression p l
    goExpression p r
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
      addEdgeMay p (l ^. slambdaVar)

    goLambda :: Lambda -> Sem r ()
    goLambda Lambda {..} = mapM_ goClause _lambdaClauses
      where
        goClause :: LambdaClause -> Sem r ()
        goClause (LambdaClause {..}) = do
          goExpression p _lambdaBody
          mapM_ (goPattern p) _lambdaPatterns

    goCase :: Case -> Sem r ()
    goCase c = do
      goExpression p (c ^. caseExpression)
      mapM_ goBranch (c ^. caseBranches)

    goBranch :: CaseBranch -> Sem r ()
    goBranch = goExpression p . (^. caseBranchExpression)

    goLet :: Let -> Sem r ()
    goLet l = do
      mapM_ goLetClause (l ^. letClauses)
      goExpression p (l ^. letExpression)

    goLetClause :: LetClause -> Sem r ()
    goLetClause = \case
      LetFunDef f -> do
        addEdgeMay p (f ^. funDefName)
        goFunctionDefHelper f
      LetMutualBlock MutualBlockLet {..} -> mapM_ goFunctionDefHelper _mutualLet

goInductiveParameter ::
  (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) =>
  Maybe Name ->
  InductiveParameter ->
  Sem r ()
goInductiveParameter p param =
  addEdgeMay p (param ^. inductiveParamName)

goFunctionParameter ::
  (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) =>
  Maybe Name ->
  FunctionParameter ->
  Sem r ()
goFunctionParameter p param = do
  whenJust (param ^. paramName) (addEdgeMay p)
  goExpression p (param ^. paramType)
