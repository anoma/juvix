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
  edgesCompose a b = fromEdgeList $ catMaybes
     [ composeEdge ea eb | ea <- toList a, eb <- toList b ]
  edgesUnion :: Edges -> Edges -> Edges
  edgesUnion = HashMap.union
  edgesCount :: Edges -> Int
  edgesCount = HashMap.size

reflexiveEdges :: CompleteCallGraph -> [ReflexiveEdge]
reflexiveEdges (CompleteCallGraph es) = mapMaybe reflexive (toList es)
  where
  reflexive :: Edge -> Maybe ReflexiveEdge
  reflexive e
   | e ^. edgeFrom == e ^. edgeTo
    = Just $ ReflexiveEdge (e ^.edgeFrom) (e ^. edgeMatrices)
   | otherwise = Nothing

callMatrixDiag :: CallMatrix -> [Rel]
callMatrixDiag m = [ col i r | (i, r) <- zip [0 :: Int ..] m]
  where
  col :: Int -> CallRow -> Rel
  col i (CallRow row) = case row of
    Nothing -> RNothing
    Just (j, r')
      | i == j -> RJust r'
      | otherwise -> RJust r'

recursiveBehaviour :: ReflexiveEdge -> RecursiveBehaviour
recursiveBehaviour re =
  RecursiveBehaviour  (re ^. redgeFun )
  (map callMatrixDiag (re ^. redgeMatrices))

findOrder :: RecursiveBehaviour -> Maybe LexOrder
findOrder rb = LexOrder <$> listToMaybe (mapMaybe (isLexOrder >=> nonEmpty) allPerms)
  where
  b0 :: [[Rel]]
  b0 = rb ^. recBehaviourMatrix
  indexed = map (zip [0 :: Int ..] . take minLength) b0
    where
    minLength =  minimum (map length b0)

  startB = removeUselessColumns indexed

    -- | removes columns that don't have at least one ≺ in them
  removeUselessColumns :: [[(Int, Rel)]] -> [[(Int, Rel)]]
  removeUselessColumns = transpose . filter (any (isLess . snd) ) . transpose

  isLexOrder :: [Int] -> Maybe [Int]
  isLexOrder = go startB
    where
    go :: [[(Int, Rel)]] -> [Int] -> Maybe [Int]
    go [] _   = Just []
    go b perm = case perm of
      [] -> error "The permutation should have one element at least!"
      (p0 : ptail)
        | Just r <- find (isLess . snd . (!! p0)) b ,
          all (notNothing . snd . (!! p0)) b ,
          Just perm' <- go (b' p0) (map pred ptail)
          -> Just ( fst (r !! p0) : perm')
        | otherwise -> Nothing
      where
      b' i = map r' (filter (not . isLess . snd . (!!i)) b)
        where
        r' r = case splitAt i r of
          (x, y) -> x ++ drop 1 y

  notNothing = (RNothing /=)
  isLess = (RJust RLe ==)

  allPerms :: [[Int]]
  allPerms = case nonEmpty startB of
    Nothing -> []
    Just s -> permutations [0 .. length (head s) - 1]
