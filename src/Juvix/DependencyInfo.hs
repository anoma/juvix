module Juvix.DependencyInfo where

import Data.Graph (Graph, Vertex)
import Data.Graph qualified as Graph
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Prelude

-- DependencyInfo is polymorphic to anticipate future use with other identifier
-- types in JuvixCore and further. The graph algorithms don't depend on the
-- exact type of names (the polymorphic type n), so there is no reason to
-- specialise DependencyInfo to any particular name type
data DependencyInfo n = DependencyInfo
  { _depInfoGraph :: Graph,
    _depInfoNodeFromVertex :: Vertex -> (n, [n]),
    _depInfoVertexFromName :: n -> Maybe Vertex,
    _depInfoReachable :: HashSet n
  }

makeLenses ''DependencyInfo

createDependencyInfo :: (Hashable n, Ord n) => HashMap n (HashSet n) -> HashSet n -> DependencyInfo n
createDependencyInfo edges startNames =
  DependencyInfo
    { _depInfoGraph = graph,
      _depInfoNodeFromVertex = \v -> case nodeFromVertex v of (_, x, y) -> (x, y),
      _depInfoVertexFromName = vertexFromName,
      _depInfoReachable = reachableNames
    }
  where
    (graph, nodeFromVertex, vertexFromName) =
      Graph.graphFromEdges $
        map (\(x, y) -> (x, x, HashSet.toList y)) (HashMap.toList edges)
    reachableNames =
      HashSet.fromList $
        map (\v -> case nodeFromVertex v of (_, x, _) -> x) $
          concatMap (Graph.reachable graph) startVertices
    startVertices = mapMaybe vertexFromName (HashSet.toList startNames)

nameFromVertex :: DependencyInfo n -> Vertex -> n
nameFromVertex depInfo v = fst $ (depInfo ^. depInfoNodeFromVertex) v

isReachable :: Hashable n => DependencyInfo n -> n -> Bool
isReachable depInfo n = HashSet.member n (depInfo ^. depInfoReachable)
