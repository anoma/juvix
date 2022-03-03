module MiniJuvix.Termination.CallGraph (
  module MiniJuvix.Termination.Types,
  module MiniJuvix.Termination.CallGraph
                                       ) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language.Extra
import qualified Data.HashMap.Strict as HashMap
import MiniJuvix.Termination.Types

multiply :: CallMatrix -> CallMatrix -> CallMatrix
multiply a b = map sumProdRow a
  where
  rowB :: Int -> CallRow
  rowB i = CallRow $ case b !? i of
    Just (CallRow (Just c)) -> Just c
    _ -> Nothing
  sumProdRow :: CallRow -> CallRow
  sumProdRow (CallRow mr) = CallRow $ do
    (ki, ra) <- mr
    (j, rb) <- _callRow (rowB ki)
    return (j, mul' ra rb)

multiplyMany :: [CallMatrix] -> [CallMatrix] -> [CallMatrix]
multiplyMany r s = [ multiply a b | a <- r, b <- s]

composeEdge :: Edge -> Edge -> Maybe Edge
composeEdge a b = do
  guard (a ^. edgeTo == b ^. edgeFrom)
  return Edge {
    _edgeFrom = a ^. edgeFrom,
    _edgeTo = b ^. edgeTo,
    _edgeMatrices = multiplyMany (a ^. edgeMatrices) (b ^. edgeMatrices)
    }

fromFunCall :: FunctionName -> FunCall -> Call
fromFunCall caller fc =
  Call {_callFrom = caller,
          _callTo = fc ^. callName,
          _callMatrix = map fst (fc ^. callArgs)
       }

completeCallGraph :: CallMap -> CompleteCallGraph
completeCallGraph cm = CompleteCallGraph (go startingEdges)
  where
  startingEdges :: Edges
  startingEdges = foldr insertCall mempty allCalls
    where
    insertCall :: Call -> Edges -> Edges
    insertCall Call {..} = HashMap.alter (Just . aux) (_callFrom, _callTo)
      where
      aux :: Maybe Edge -> Edge
      aux me = case me of
        Nothing -> Edge _callFrom _callTo [_callMatrix]
        Just e -> over edgeMatrices (_callMatrix : ) e
  allCalls :: [Call]
  allCalls = [ fromFunCall caller funCall
               | (caller, callerMap) <- HashMap.toList (cm ^. callMap),
               (_, funCalls) <- HashMap.toList callerMap,
               funCall <- funCalls ]
  go :: Edges -> Edges
  go m
    | edgesCount m == edgesCount m' = m
    | otherwise = go m'
    where
    m' = step m
  step :: Edges -> Edges
  step s = edgesUnion s (edgesCompose s startingEdges)
  fromEdgeList :: [Edge] -> Edges
  fromEdgeList l = HashMap.fromList [ ((e ^. edgeFrom, e ^. edgeTo), e) | e <- l]
  edgesCompose :: Edges -> Edges -> Edges
  edgesCompose a b = fromEdgeList $ catMaybes [ composeEdge ea eb | ea <- toList a, eb <- toList b ]
  edgesUnion :: Edges -> Edges -> Edges
  edgesUnion = HashMap.union
  edgesCount :: Edges -> Int
  edgesCount = HashMap.size
