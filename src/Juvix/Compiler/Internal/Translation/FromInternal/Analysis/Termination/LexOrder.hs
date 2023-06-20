module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.LexOrder
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.LexOrder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Prelude

fromEdgeList :: [Edge] -> EdgeMap
fromEdgeList l = HashMap.fromList [((e ^. edgeFrom, e ^. edgeTo), e) | e <- l]

composeEdge :: Edge -> Edge -> Maybe Edge
composeEdge a b = do
  guard (a ^. edgeTo == b ^. edgeFrom)
  return
    Edge
      { _edgeFrom = a ^. edgeFrom,
        _edgeTo = b ^. edgeTo,
        _edgeMatrices = multiplyMany (a ^. edgeMatrices) (b ^. edgeMatrices)
      }

edgesCompose :: EdgeMap -> EdgeMap -> EdgeMap
edgesCompose g h =
  fromEdgeList
    (catMaybes [composeEdge u v | u <- toList g, v <- toList h])

edgeUnion :: Edge -> Edge -> Edge
edgeUnion a b
  | a ^. edgeFrom == b ^. edgeFrom,
    a ^. edgeTo == b ^. edgeTo =
      Edge
        (a ^. edgeFrom)
        (a ^. edgeTo)
        (HashSet.union (a ^. edgeMatrices) (b ^. edgeMatrices))
  | otherwise = impossible

edgesUnion :: EdgeMap -> EdgeMap -> EdgeMap
edgesUnion = HashMap.unionWith edgeUnion

edgesCount :: EdgeMap -> Int
edgesCount es = sum [HashSet.size (e ^. edgeMatrices) | e <- toList es]

multiply :: CallMatrix -> CallMatrix -> CallMatrix
multiply (CallMatrix a) (CallMatrix b) = CallMatrix (map sumProdRow a)
  where
    rowB :: Int -> CallRow
    rowB i = CallRow $ case b !? i of
      Just (CallRow (Just c)) -> Just c
      _ -> Nothing

    sumProdRow :: CallRow -> CallRow
    sumProdRow (CallRow mr) = CallRow $ do
      (ki, ra) <- mr
      (j, rb) <- rowB ki ^. callRow
      return (j, mul' ra rb)

multiplyMany :: HashSet CallMatrix -> HashSet CallMatrix -> HashSet CallMatrix
multiplyMany r s = HashSet.fromList [multiply a b | a <- toList r, b <- toList s]

fromFunCall :: FunctionRef -> FunCall -> Call
fromFunCall caller fc =
  Call
    { _callFrom = caller,
      _callTo = fc ^. callRef,
      _callMatrix = CallMatrix (map fst (fc ^. callArgs))
    }

-- | IMPORTANT: the resulting call graph is not complete. Use this function
-- only to filter the pretty printed graph
unsafeFilterGraph :: (Foldable f) => f Text -> CompleteCallGraph -> CompleteCallGraph
unsafeFilterGraph funNames (CompleteCallGraph g) =
  CompleteCallGraph (HashMap.filterWithKey (\(f, _) _ -> f ^. nameText `elem` funNames) g)

completeCallGraph :: CallMap -> CompleteCallGraph
completeCallGraph CallMap {..} = CompleteCallGraph (go startingEdges)
  where
    startingEdges :: EdgeMap
    startingEdges = foldr insertCall mempty allCalls
      where
        insertCall :: Call -> EdgeMap -> EdgeMap
        insertCall Call {..} = HashMap.alter (Just . aux) (_callFrom, _callTo)
          where
            aux :: Maybe Edge -> Edge
            aux me = case me of
              Nothing -> Edge _callFrom _callTo (HashSet.singleton _callMatrix)
              Just e -> over edgeMatrices (HashSet.insert _callMatrix) e

    allCalls :: [Call]
    allCalls =
      [ fromFunCall caller funCall
        | (caller, callerMap) <- HashMap.toList _callMap,
          (_, funCalls) <- HashMap.toList callerMap,
          funCall <- funCalls
      ]

    go :: EdgeMap -> EdgeMap
    go g
      | edgesCount g == edgesCount g' = g
      | otherwise = go g'
      where
        g' = step g

    step :: EdgeMap -> EdgeMap
    step s = edgesUnion (edgesCompose s startingEdges) s

reflexiveEdges :: CompleteCallGraph -> [ReflexiveEdge]
reflexiveEdges (CompleteCallGraph es) = mapMaybe reflexive (toList es)
  where
    reflexive :: Edge -> Maybe ReflexiveEdge
    reflexive e
      | e ^. edgeFrom == e ^. edgeTo =
          Just $ ReflexiveEdge (e ^. edgeFrom) (e ^. edgeMatrices)
      | otherwise = Nothing

callMatrixDiag :: CallMatrix -> [SizeRel]
callMatrixDiag (CallMatrix m) = [col i r | (i, r) <- zip [0 :: Int ..] m]
  where
    col :: Int -> CallRow -> SizeRel
    col i (CallRow row) = case row of
      Nothing -> RNothing
      Just (j, r')
        | i == j -> RJust r'
        | otherwise -> RNothing

recursiveBehaviour :: ReflexiveEdge -> RecursiveBehaviour
recursiveBehaviour re =
  RecursiveBehaviour
    (re ^. reflexiveEdgeFun)
    (map callMatrixDiag (toList $ re ^. reflexiveEdgeMatrices))

findOrder :: RecursiveBehaviour -> Maybe LexOrder
findOrder rb = LexOrder <$> listToMaybe (mapMaybe (isLexOrder >=> nonEmpty) allPerms)
  where
    b0 :: [[SizeRel]]
    b0 = rb ^. recursiveBehaviourMatrix

    indexed = map (zip [0 :: Int ..] . take minLength) b0
      where
        minLength = minimum (map length b0)

    startB = removeUselessColumns indexed

    -- removes columns that don't have at least one ≺ in them
    removeUselessColumns :: [[(Int, SizeRel)]] -> [[(Int, SizeRel)]]
    removeUselessColumns = transpose . filter (any (isLess . snd)) . transpose

    isLexOrder :: [Int] -> Maybe [Int]
    isLexOrder = go startB
      where
        go :: [[(Int, SizeRel)]] -> [Int] -> Maybe [Int]
        go [] _ = Just []
        go b perm = case perm of
          [] -> error "The permutation should have one element at least!"
          (p0 : ptail)
            | Just r <- find (isLess . snd . (!! p0)) b,
              all (notNothing . snd . (!! p0)) b,
              Just perm' <- go (b' p0) (map pred ptail) ->
                Just (fst (r !! p0) : perm')
            | otherwise -> Nothing
          where
            b' i = map r' (filter (not . isLess . snd . (!! i)) b)
              where
                r' r = case splitAt i r of
                  (x, y) -> x ++ drop 1 y

    notNothing = (RNothing /=)
    isLess = (RJust RLe ==)

    allPerms :: [[Int]]
    allPerms = case nonEmpty startB of
      Nothing -> []
      Just s -> permutations [0 .. length (head s) - 1]
