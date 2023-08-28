module Juvix.Data.DependencyInfo where

import Data.Graph qualified as Graph
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data DependencyInfo n = DependencyInfo
  { _depInfoGraph :: Graph,
    _depInfoNodeFromVertex :: Vertex -> (n, HashSet n),
    _depInfoEdgeList :: [(n, n, [n])],
    _depInfoVertexFromName :: n -> Maybe Vertex,
    _depInfoReachable :: HashSet n,
    _depInfoTopSort :: [n]
  }

makeLenses ''DependencyInfo

instance (Pretty n) => Pretty (DependencyInfo n) where
  pretty n = pretty (map helper (n ^. depInfoEdgeList))
    where
      helper :: (n, n, [n]) -> (n, [n])
      helper (a, _, c) = (a, c)

createDependencyInfo :: forall n. (Hashable n, Ord n) => HashMap n (HashSet n) -> HashSet n -> DependencyInfo n
createDependencyInfo edges startNames =
  DependencyInfo
    { _depInfoGraph = graph,
      _depInfoNodeFromVertex = \v -> let (_, x, y) = nodeFromVertex v in (x, HashSet.fromList y),
      _depInfoEdgeList = edgeList,
      _depInfoVertexFromName = vertexFromName,
      _depInfoReachable = reachableNames,
      _depInfoTopSort = topSortedNames
    }
  where
    graph :: Graph
    nodeFromVertex :: Vertex -> (n, n, [n])
    vertexFromName :: n -> Maybe Vertex
    (graph, nodeFromVertex, vertexFromName) = Graph.graphFromEdges edgeList
    edgeList :: [(n, n, [n])]
    edgeList = map (\(x, y) -> (x, x, HashSet.toList y)) (HashMap.toList edges)
    reachableNames :: HashSet n
    reachableNames =
      HashSet.fromList $
        map (\v -> case nodeFromVertex v of (_, x, _) -> x) $
          concatMap (Graph.reachable graph) startVertices
    startVertices :: [Vertex]
    startVertices = mapMaybe vertexFromName (HashSet.toList startNames)
    topSortedNames :: [n]
    topSortedNames = map (\v -> case nodeFromVertex v of (_, n, _) -> n) (Graph.topSort graph)

nameFromVertex :: DependencyInfo n -> Vertex -> n
nameFromVertex depInfo = fst . (depInfo ^. depInfoNodeFromVertex)

isReachable :: (Hashable n) => DependencyInfo n -> n -> Bool
isReachable depInfo n = HashSet.member n (depInfo ^. depInfoReachable)

isPath :: DependencyInfo n -> n -> n -> Bool
isPath depInfo n n' = Graph.path (depInfo ^. depInfoGraph) v v'
  where
    v = fromJust $ (depInfo ^. depInfoVertexFromName) n
    v' = fromJust $ (depInfo ^. depInfoVertexFromName) n'

buildSCCs :: (Ord n) => DependencyInfo n -> [SCC n]
buildSCCs = Graph.stronglyConnComp . (^. depInfoEdgeList)

isCyclic :: (Ord n) => DependencyInfo n -> Bool
isCyclic = any (\case CyclicSCC _ -> True; _ -> False) . buildSCCs

nodesOnCycles :: forall n. (Hashable n, Ord n) => DependencyInfo n -> HashSet n
nodesOnCycles = foldr go mempty . buildSCCs
  where
    go :: SCC n -> HashSet n -> HashSet n
    go x acc = case x of
      CyclicSCC ns -> foldr HashSet.insert acc ns
      _ -> acc
