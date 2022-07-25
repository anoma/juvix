module Juvix.Syntax.Abstract.DependencyBuilder (buildDependencyInfo) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Prelude
import Juvix.Syntax.Abstract.Language
import Juvix.Syntax.Abstract.NameDependencyInfo

-- adjacency set representation
type DependencyGraph = HashMap Name (HashSet Name)

type StartNodes = HashSet Name

type VisitedModules = HashSet Name

type ExportsTable = HashSet NameId

buildDependencyInfo :: NonEmpty TopModule -> ExportsTable -> NameDependencyInfo
buildDependencyInfo ms tab =
  createDependencyInfo graph startNodes
  where
    (startNodes, graph) =
      run $
        evalState (HashSet.empty :: VisitedModules) $
          runState HashSet.empty $
            execState HashMap.empty $
              runReader tab $
                mapM_ goModule ms

addStartNode :: Member (State StartNodes) r => Name -> Sem r ()
addStartNode n = modify (HashSet.insert n)

addEdge :: Member (State DependencyGraph) r => Name -> Name -> Sem r ()
addEdge n1 n2 =
  modify
    ( HashMap.alter
        ( \case
            Just ns -> Just (HashSet.insert n2 ns)
            Nothing -> Just (HashSet.singleton n2)
        )
        n1
    )

checkStartNode :: Members '[Reader ExportsTable, State StartNodes] r => Name -> Sem r ()
checkStartNode n = do
  tab <- ask
  if
      | HashSet.member (n ^. nameId) tab -> addStartNode n
      | otherwise -> return ()

guardNotVisited :: Member (State VisitedModules) r => Name -> Sem r () -> Sem r ()
guardNotVisited n cont = do
  s <- get
  if
      | HashSet.member n s -> return ()
      | otherwise -> do
          put (HashSet.insert n s)
          cont

goModule :: Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, State VisitedModules] r => Module -> Sem r ()
goModule m = do
  checkStartNode (m ^. moduleName)
  mapM_ (goStatement (m ^. moduleName)) (m ^. (moduleBody . moduleStatements))

goLocalModule :: Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, State VisitedModules] r => Name -> Module -> Sem r ()
goLocalModule mn m = do
  addEdge (m ^. moduleName) mn
  goModule m

-- declarations in a module depend on the module, not the other way round (a
-- module is reachable if at least one of the declarations in it is reachable)
goStatement :: Members '[Reader ExportsTable, State DependencyGraph, State StartNodes, State VisitedModules] r => Name -> Statement -> Sem r ()
goStatement mn = \case
  StatementAxiom ax -> do
    checkStartNode (ax ^. axiomName)
    addEdge (ax ^. axiomName) mn
    goExpression (ax ^. axiomName) (ax ^. axiomType)
  StatementForeign {} -> return ()
  StatementFunction f -> do
    checkStartNode (f ^. funDefName)
    addEdge (f ^. funDefName) mn
    goExpression (f ^. funDefName) (f ^. funDefTypeSig)
    mapM_ (goFunctionClause (f ^. funDefName)) (f ^. funDefClauses)
  StatementImport m -> guardNotVisited (m ^. moduleName) (goModule m)
  StatementLocalModule m -> goLocalModule mn m
  StatementInductive i -> do
    checkStartNode (i ^. inductiveName)
    addEdge (i ^. inductiveName) mn
    mapM_ (goFunctionParameter (i ^. inductiveName)) (i ^. inductiveParameters)
    goExpression (i ^. inductiveName) (i ^. inductiveType)
    mapM_ (goConstructorDef (i ^. inductiveName)) (i ^. inductiveConstructors)

-- constructors of an inductive type depend on the inductive type, not the other
-- way round
goConstructorDef :: Member (State DependencyGraph) r => Name -> InductiveConstructorDef -> Sem r ()
goConstructorDef indName c = do
  addEdge (c ^. constructorName) indName
  goExpression (c ^. constructorName) (c ^. constructorType)

goFunctionClause :: Member (State DependencyGraph) r => Name -> FunctionClause -> Sem r ()
goFunctionClause p c = goExpression p (c ^. clauseBody)

goExpression :: Member (State DependencyGraph) r => Name -> Expression -> Sem r ()
goExpression p e = case e of
  ExpressionIden i -> addEdge p (getIdenName i)
  ExpressionUniverse {} -> return ()
  ExpressionFunction f -> do
    goFunctionParameter p (f ^. funParameter)
    goExpression p (f ^. funReturn)
  ExpressionApplication (Application l r _) -> do
    goExpression p l
    goExpression p r
  ExpressionLiteral {} -> return ()
  ExpressionHole {} -> return ()

goFunctionParameter :: Member (State DependencyGraph) r => Name -> FunctionParameter -> Sem r ()
goFunctionParameter p param = goExpression p (param ^. paramType)
