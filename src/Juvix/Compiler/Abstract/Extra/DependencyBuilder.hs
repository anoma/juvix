module Juvix.Compiler.Abstract.Extra.DependencyBuilder
  ( buildDependencyInfo,
    buildDependencyInfoPreModule,
    buildDependencyInfoExpr,
    ExportsTable,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Data.NameDependencyInfo
import Juvix.Compiler.Abstract.Extra.Functions
import Juvix.Compiler.Abstract.Language
import Juvix.Prelude

-- | Adjacency set representation
type DependencyGraph = HashMap Name (HashSet Name)

type StartNodes = HashSet Name

type ExportsTable = HashSet NameId

buildDependencyInfoPreModule :: PreModule -> ExportsTable -> NameDependencyInfo
buildDependencyInfoPreModule ms tab =
  buildDependencyInfoHelper tab (goPreModule ms)

buildDependencyInfo :: NonEmpty Module -> ExportsTable -> NameDependencyInfo
buildDependencyInfo ms tab =
  buildDependencyInfoHelper tab (goModule ms)

buildDependencyInfoExpr :: Expression -> NameDependencyInfo
buildDependencyInfoExpr = buildDependencyInfoHelper mempty . goExpression Nothing

buildDependencyInfoHelper ::
  ExportsTable ->
  Sem '[Reader ExportsTable, State DependencyGraph, State StartNodes] () ->
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

-- BuiltinBool and BuiltinNat are required by the Internal to Core translation
-- when translating literal integers to Nats.
checkBuiltinInductiveStartNode :: forall r. Member (State StartNodes) r => InductiveDef -> Sem r ()
checkBuiltinInductiveStartNode i = whenJust (i ^. inductiveBuiltin) go
  where
    go :: BuiltinInductive -> Sem r ()
    go = \case
      BuiltinNat -> addInductiveStartNode
      BuiltinBool -> addInductiveStartNode
      BuiltinInt -> addInductiveStartNode

    addInductiveStartNode :: Sem r ()
    addInductiveStartNode = addStartNode (i ^. inductiveName)

goPreModule :: Members '[Reader ExportsTable, State DependencyGraph, State StartNodes] r => PreModule -> Sem r ()
goPreModule m = do
  checkStartNode (m ^. moduleName)
  let b = m ^. moduleBody
  -- TODO ignoring includes... is this ok?
  mapM_ (goPreStatement (m ^. moduleName)) (b ^. moduleStatements)

-- | Declarations in a module depend on the module, not the other way round (a
-- module is reachable if at least one of the declarations in it is reachable)
goPreStatement :: forall r. Members '[Reader ExportsTable, State DependencyGraph, State StartNodes] r => Name -> PreStatement -> Sem r ()
goPreStatement parentModule = \case
  PreAxiomDef ax -> goAxiom ax
  PreFunctionDef f -> goTopFunctionDef parentModule f
  PreInductiveDef i -> goInductive i
  where
    goAxiom :: AxiomDef -> Sem r ()
    goAxiom ax = do
      checkStartNode (ax ^. axiomName)
      addEdge (ax ^. axiomName) parentModule
      goExpression (Just (ax ^. axiomName)) (ax ^. axiomType)

    goInductive :: InductiveDef -> Sem r ()
    goInductive i = do
      checkStartNode (i ^. inductiveName)
      checkBuiltinInductiveStartNode i
      addEdge (i ^. inductiveName) parentModule
      mapM_ (goInductiveParameter (Just (i ^. inductiveName))) (i ^. inductiveParameters)
      goExpression (Just (i ^. inductiveName)) (i ^. inductiveType)
      mapM_ (goConstructorDef (i ^. inductiveName)) (i ^. inductiveConstructors)

goTopFunctionDef :: (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> FunctionDef -> Sem r ()
goTopFunctionDef modName f = do
  addEdge (f ^. funDefName) modName
  goFunctionDefHelper f

goFunctionDefHelper ::
  (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) =>
  FunctionDef ->
  Sem r ()
goFunctionDefHelper f = do
  checkStartNode (f ^. funDefName)
  goExpression (Just (f ^. funDefName)) (f ^. funDefTypeSig)
  mapM_ (goFunctionClause (f ^. funDefName)) (f ^. funDefClauses)

-- constructors of an inductive type depend on the inductive type, not the other
-- way round; an inductive type depends on the types of its constructors
goConstructorDef :: (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> InductiveConstructorDef -> Sem r ()
goConstructorDef indName c = do
  addEdge (c ^. constructorName) indName
  goExpression (Just indName) (c ^. constructorType)

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
    goFunctionParameter p (f ^. funParameter)
    goExpression p (f ^. funReturn)
  ExpressionApplication (Application l r _) -> do
    goExpression p l
    goExpression p r
  ExpressionLiteral {} -> return ()
  ExpressionCase c -> goCase c
  ExpressionHole {} -> return ()
  ExpressionLambda l -> goLambda l
  ExpressionLet l -> goLet l
  where
    goLambda :: Lambda -> Sem r ()
    goLambda (Lambda clauses) = mapM_ goClause clauses
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
