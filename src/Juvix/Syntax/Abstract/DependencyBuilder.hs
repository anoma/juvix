module Juvix.Syntax.Abstract.DependencyBuilder (buildDependencyInfo) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.DependencyInfo
import Juvix.Prelude
import Juvix.Syntax.Abstract.Language

-- adjacency list representation
type DependencyGraph = HashMap Name [Name]

type VisitedModules = HashSet Name

buildDependencyInfo :: [TopModule] -> DependencyInfo Name
buildDependencyInfo ms =
  createDependencyInfo
    (fst (run (evalState (HashSet.empty :: VisitedModules) (runState HashMap.empty (mapM_ goModule ms)))))

addEdge :: Member (State DependencyGraph) r => Name -> Name -> Sem r ()
addEdge n1 n2 =
  modify
    ( HashMap.alter
        ( \case
            Just ns -> Just (n2 : ns)
            Nothing -> Just [n2]
        )
        n1
    )

guardNotVisited :: Member (State VisitedModules) r => Name -> Sem r () -> Sem r ()
guardNotVisited n cont = do
  s <- get
  if
      | HashSet.member n s -> return ()
      | otherwise -> do
          put (HashSet.insert n s)
          cont

goModule :: Members '[State DependencyGraph, State VisitedModules] r => Module -> Sem r ()
goModule m =
  mapM_ (goStatement (m ^. moduleName)) (m ^. (moduleBody . moduleStatements))

goLocalModule :: Members '[State DependencyGraph, State VisitedModules] r => Name -> Module -> Sem r ()
goLocalModule mn m = do
  addEdge (m ^. moduleName) mn
  goModule m

-- declarations in a module depend on the module, not the other way round (a
-- module is reachable if at least one of the declarations in it is reachable)
goStatement :: Members '[State DependencyGraph, State VisitedModules] r => Name -> Statement -> Sem r ()
goStatement mn = \case
  StatementAxiom ax -> do
    addEdge (ax ^. axiomName) mn
    goExpression (ax ^. axiomName) (ax ^. axiomType)
  StatementForeign {} -> return ()
  StatementFunction f -> do
    addEdge (f ^. funDefName) mn
    mapM_ (goFunctionClause (f ^. funDefName)) (f ^. funDefClauses)
  StatementImport m -> guardNotVisited (m ^. moduleName) (goModule m)
  StatementLocalModule m -> goLocalModule mn m
  StatementInductive i -> do
    addEdge (i ^. inductiveName) mn
    mapM_ (goFunctionParameter (i ^. inductiveName)) (i ^. inductiveParameters)
    goExpression (i ^. inductiveName) (i ^. inductiveType)
    mapM_ (goConstructorDef (i ^. inductiveName)) (i ^. inductiveConstructors)

goFunctionClause :: Members '[State DependencyGraph, State VisitedModules] r => Name -> FunctionClause -> Sem r ()
goFunctionClause p c = goExpression p (c ^. clauseBody)

goExpression :: Members '[State DependencyGraph, State VisitedModules] r => Name -> Expression -> Sem r ()
goExpression p e = case e of
  ExpressionIden i -> addEdge p (getName i)
  ExpressionUniverse {} -> return ()
  ExpressionFunction f -> do
    goFunctionParameter p (f ^. funParameter)
    goExpression p (f ^. funReturn)
  ExpressionApplication (Application l r _) -> do
    goExpression p l
    goExpression p r
  ExpressionLiteral {} -> return ()
  ExpressionHole {} -> return ()

goFunctionParameter :: Members '[State DependencyGraph, State VisitedModules] r => Name -> FunctionParameter -> Sem r ()
goFunctionParameter p param = goExpression p (param ^. paramType)

-- constructors of an inductive type depend on the inductive type, not the other
-- way round
goConstructorDef :: Members '[State DependencyGraph, State VisitedModules] r => Name -> InductiveConstructorDef -> Sem r ()
goConstructorDef indName c = do
  addEdge (c ^. constructorName) indName
  goExpression (c ^. constructorName) (c ^. constructorType)
