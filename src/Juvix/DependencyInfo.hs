module Juvix.DependencyInfo where

import Data.Graph (Graph, Vertex)
import Data.Graph qualified as Graph
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Prelude

-- DependencyInfo is polymorphic to anticipate future use with other identifier
-- types in JuvixCore and further. The graph algorithms don't depend on the
-- exact type of names (the polymorphic type n), so I see no reason to
-- specialise DependencyInfo to any particular name type
data DependencyInfo n = DependencyInfo
  { _depInfoGraph :: Graph,
    _depInfoNodeFromVertex :: Vertex -> (n, [n]),
    _depInfoVertexFromName :: n -> Maybe Vertex,
    _depInfoReachable :: Maybe (HashSet n)
  }

makeLenses ''DependencyInfo

createDependencyInfo :: Ord n => HashMap n [n] -> DependencyInfo n
createDependencyInfo edges =
  DependencyInfo
    { _depInfoGraph = graph,
      _depInfoNodeFromVertex = \v -> case nodeFromVertex v of (_, x, y) -> (x, y),
      _depInfoVertexFromName = vertexFromName,
      _depInfoReachable = Nothing
    }
  where
    (graph, nodeFromVertex, vertexFromName) =
      Graph.graphFromEdges $
        map (\(x, y) -> (x, x, nubSort y)) (HashMap.toList edges)

nameFromVertex :: DependencyInfo n -> Vertex -> n
nameFromVertex depInfo v = fst $ (depInfo ^. depInfoNodeFromVertex) v

computeReachability :: Hashable n => DependencyInfo n -> [n] -> DependencyInfo n
computeReachability depInfo startNames =
  depInfo {_depInfoReachable = Just reachableNames}
  where
    reachableNames =
      HashSet.fromList $
        map (nameFromVertex depInfo) $
          concatMap (Graph.reachable (depInfo ^. depInfoGraph)) startVertices
    startVertices = mapMaybe (depInfo ^. depInfoVertexFromName) startNames

isReachable :: Hashable n => DependencyInfo n -> n -> Bool
isReachable depInfo n = HashSet.member n (fromMaybe err (depInfo ^. depInfoReachable))
  where
    err :: a
    err = error "call `computeReachability` before using `isReachable`"
