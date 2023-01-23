module Juvix.Compiler.Abstract.Extra.DependencyBuilder (buildDependencyInfo, ExportsTable) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Data.NameDependencyInfo
import Juvix.Compiler.Abstract.Extra.Functions
import Juvix.Compiler.Abstract.Language
import Juvix.Prelude

-- adjacency set representation
type DependencyGraph = HashMap Name (HashSet Name)

type StartNodes = HashSet Name

type VisitedModules = HashSet Name

type ExportsTable = HashSet NameId

buildDependencyInfo :: NonEmpty TopModule -> ExportsTable -> NameDependencyInfo
buildDependencyInfo ms tab =
  createDependencyInfo graph startNodes
  where
    startNodes :: StartNodes
    graph :: DependencyGraph
    (startNodes, graph) =
      run $
        evalState (HashSet.empty :: VisitedModules) $
          runState HashSet.empty $
            execState HashMap.empty $
              runReader tab $
                mapM_ goModule ms

addStartNode :: (Member (State StartNodes) r) => Name -> Sem r ()
addStartNode n = modify (HashSet.insert n)

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

guardNotVisited :: (Member (State VisitedModules) r) => Name -> Sem r () -> Sem r ()
guardNotVisited n cont =
  unlessM
    (HashSet.member n <$> get)
    (modify (HashSet.insert n) >> cont)

goModule :: (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, State VisitedModules] r) => Module -> Sem r ()
goModule m = do
  checkStartNode (m ^. moduleName)
  mapM_ (goStatement (m ^. moduleName)) (m ^. (moduleBody . moduleStatements))

goLocalModule :: (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, State VisitedModules] r) => Name -> Module -> Sem r ()
goLocalModule mn m = do
  addEdge (m ^. moduleName) mn
  goModule m

-- declarations in a module depend on the module, not the other way round (a
-- module is reachable if at least one of the declarations in it is reachable)
goStatement :: (Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, State VisitedModules] r) => Name -> Statement -> Sem r ()
goStatement modName = \case
  StatementAxiom ax -> do
    checkStartNode (ax ^. axiomName)
    addEdge (ax ^. axiomName) modName
    goExpression (ax ^. axiomName) (ax ^. axiomType)
  StatementForeign {} -> return ()
  StatementFunction f -> goTopFunctionDef modName f
  StatementImport m -> guardNotVisited (m ^. moduleName) (goModule m)
  StatementLocalModule m -> goLocalModule modName m
  StatementInductive i -> do
    checkStartNode (i ^. inductiveName)
    addEdge (i ^. inductiveName) modName
    mapM_ (goFunctionParameter (i ^. inductiveName)) (i ^. inductiveParameters)
    goExpression (i ^. inductiveName) (i ^. inductiveType)
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
  goExpression (f ^. funDefName) (f ^. funDefTypeSig)
  mapM_ (goFunctionClause (f ^. funDefName)) (f ^. funDefClauses)

-- constructors of an inductive type depend on the inductive type, not the other
-- way round; an inductive type depends on the types of its constructors
goConstructorDef :: (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> InductiveConstructorDef -> Sem r ()
goConstructorDef indName c = do
  addEdge (c ^. constructorName) indName
  goExpression indName (c ^. constructorType)

goFunctionClause :: (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> FunctionClause -> Sem r ()
goFunctionClause p c = do
  mapM_ (goPattern p) (c ^. clausePatterns)
  goExpression p (c ^. clauseBody)

goPattern :: forall r. (Member (State DependencyGraph) r) => Name -> PatternArg -> Sem r ()
goPattern n p = case p ^. patternArgPattern of
  PatternVariable {} -> return ()
  PatternWildcard {} -> return ()
  PatternEmpty -> return ()
  PatternConstructorApp a -> goApp a
  where
    goApp :: ConstructorApp -> Sem r ()
    goApp (ConstructorApp ctr ps) = do
      addEdge n (ctr ^. constructorRefName)
      mapM_ (goPattern n) ps

goExpression :: forall r. (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> Expression -> Sem r ()
goExpression p e = case e of
  ExpressionIden i -> addEdge p (idenName i)
  ExpressionUniverse {} -> return ()
  ExpressionFunction f -> do
    goFunctionParameter p (f ^. funParameter)
    goExpression p (f ^. funReturn)
  ExpressionApplication (Application l r _) -> do
    goExpression p l
    goExpression p r
  ExpressionLiteral {} -> return ()
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
          mapM_ (goPattern p) _lambdaParameters

    goLet :: Let -> Sem r ()
    goLet l = do
      mapM_ goLetClause (l ^. letClauses)
      goExpression p (l ^. letExpression)

    goLetClause :: LetClause -> Sem r ()
    goLetClause = \case
      LetFunDef f -> do
        addEdge p (f ^. funDefName)
        goFunctionDefHelper f

goFunctionParameter :: (Members '[State DependencyGraph, State StartNodes, Reader ExportsTable] r) => Name -> FunctionParameter -> Sem r ()
goFunctionParameter p param = goExpression p (param ^. paramType)
