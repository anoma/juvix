module MiniJuvix.Termination.CallGraph
  ( module MiniJuvix.Termination.Types,
    module MiniJuvix.Termination.CallGraph,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language.Extra
import MiniJuvix.Syntax.Abstract.Pretty.Base
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Termination.Types
import Prettyprinter as PP

type Edges = HashMap (FunctionName, FunctionName) Edge

data Edge = Edge
  { _edgeFrom :: FunctionName,
    _edgeTo :: FunctionName,
    _edgeMatrices :: HashSet CallMatrix
  }

newtype CompleteCallGraph = CompleteCallGraph Edges

data ReflexiveEdge = ReflexiveEdge
  { _redgeFun :: FunctionName,
    _redgeMatrices :: HashSet CallMatrix
  }

data RecursiveBehaviour = RecursiveBehaviour
  { _recBehaviourFunction :: FunctionName,
    _recBehaviourMatrix :: [[Rel]]
  }

makeLenses ''RecursiveBehaviour
makeLenses ''Edge
makeLenses ''ReflexiveEdge

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

multiplyMany :: HashSet CallMatrix -> HashSet CallMatrix -> HashSet CallMatrix
multiplyMany r s = HashSet.fromList [multiply a b | a <- toList r, b <- toList s]

composeEdge :: Edge -> Edge -> Maybe Edge
composeEdge a b = do
  guard (a ^. edgeTo == b ^. edgeFrom)
  return
    Edge
      { _edgeFrom = a ^. edgeFrom,
        _edgeTo = b ^. edgeTo,
        _edgeMatrices = multiplyMany (a ^. edgeMatrices) (b ^. edgeMatrices)
      }

fromFunCall :: FunctionName -> FunCall -> Call
fromFunCall caller fc =
  Call
    { _callFrom = caller,
      _callTo = fc ^. callName,
      _callMatrix = map fst (fc ^. callArgs)
    }

-- | IMPORTANT: the resulting call graph is not complete. Use this function
-- only to filter the pretty printed graph
unsafeFilterGraph :: Foldable f => f Text -> CompleteCallGraph -> CompleteCallGraph
unsafeFilterGraph funNames (CompleteCallGraph g) =
  CompleteCallGraph (HashMap.filterWithKey (\(f, _) _ -> S.symbolText f `elem` funNames) g)

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
              Nothing -> Edge _callFrom _callTo (HashSet.singleton _callMatrix)
              Just e -> over edgeMatrices (HashSet.insert _callMatrix) e
    allCalls :: [Call]
    allCalls =
      [ fromFunCall caller funCall
        | (caller, callerMap) <- HashMap.toList (cm ^. callMap),
          (_, funCalls) <- HashMap.toList callerMap,
          funCall <- funCalls
      ]

    go :: Edges -> Edges
    go m
      | edgesCount m == edgesCount m' = m
      | otherwise = go m'
      where
        m' = step m

    step :: Edges -> Edges
    step s = edgesUnion (edgesCompose s startingEdges) s

    fromEdgeList :: [Edge] -> Edges
    fromEdgeList l = HashMap.fromList [((e ^. edgeFrom, e ^. edgeTo), e) | e <- l]

    edgesCompose :: Edges -> Edges -> Edges
    edgesCompose a b =
      fromEdgeList $
        catMaybes
          [composeEdge ea eb | ea <- toList a, eb <- toList b]

    edgeUnion :: Edge -> Edge -> Edge
    edgeUnion a b
      | a ^. edgeFrom == b ^. edgeFrom,
        a ^. edgeTo == b ^. edgeTo =
        Edge
          (a ^. edgeFrom)
          (a ^. edgeTo)
          (HashSet.union (a ^. edgeMatrices) (b ^. edgeMatrices))
      | otherwise = impossible

    edgesUnion :: Edges -> Edges -> Edges
    edgesUnion = HashMap.unionWith edgeUnion

    edgesCount :: Edges -> Int
    edgesCount es = sum [HashSet.size (e ^. edgeMatrices) | e <- toList es]

reflexiveEdges :: CompleteCallGraph -> [ReflexiveEdge]
reflexiveEdges (CompleteCallGraph es) = mapMaybe reflexive (toList es)
  where
    reflexive :: Edge -> Maybe ReflexiveEdge
    reflexive e
      | e ^. edgeFrom == e ^. edgeTo =
        Just $ ReflexiveEdge (e ^. edgeFrom) (e ^. edgeMatrices)
      | otherwise = Nothing

callMatrixDiag :: CallMatrix -> [Rel]
callMatrixDiag m = [col i r | (i, r) <- zip [0 :: Int ..] m]
  where
    col :: Int -> CallRow -> Rel
    col i (CallRow row) = case row of
      Nothing -> RNothing
      Just (j, r')
        | i == j -> RJust r'
        | otherwise -> RNothing

recursiveBehaviour :: ReflexiveEdge -> RecursiveBehaviour
recursiveBehaviour re =
  RecursiveBehaviour
    (re ^. redgeFun)
    (map callMatrixDiag (toList $ re ^. redgeMatrices))

findOrder :: RecursiveBehaviour -> Maybe LexOrder
findOrder rb = LexOrder <$> listToMaybe (mapMaybe (isLexOrder >=> nonEmpty) allPerms)
  where
    b0 :: [[Rel]]
    b0 = rb ^. recBehaviourMatrix
    indexed = map (zip [0 :: Int ..] . take minLength) b0
      where
        minLength = minimum (map length b0)

    startB = removeUselessColumns indexed

    -- removes columns that don't have at least one ≺ in them
    removeUselessColumns :: [[(Int, Rel)]] -> [[(Int, Rel)]]
    removeUselessColumns = transpose . filter (any (isLess . snd)) . transpose

    isLexOrder :: [Int] -> Maybe [Int]
    isLexOrder = go startB
      where
        go :: [[(Int, Rel)]] -> [Int] -> Maybe [Int]
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

instance PrettyCode Edge where
  ppCode Edge {..} = do
    fromFun <- ppSCode _edgeFrom
    toFun <- ppSCode _edgeTo
    matrices <- indent 2 . ppMatrices . zip [0 :: Int ..] <$> mapM ppCode (toList _edgeMatrices)
    return $
      pretty ("Edge" :: Text) <+> fromFun <+> waveFun <+> toFun <> line
        <> matrices
    where
      ppMatrices = vsep2 . map ppMatrix
      ppMatrix (i, t) =
        pretty ("Matrix" :: Text) <+> pretty i <> colon <> line
          <> t

instance PrettyCode CompleteCallGraph where
  ppCode :: forall r. Members '[Reader Options] r => CompleteCallGraph -> Sem r (Doc Ann)
  ppCode (CompleteCallGraph edges) = do
    es <- vsep2 <$> mapM ppCode (toList edges)
    return $ pretty ("Complete Call Graph:" :: Text) <> line <> es

instance PrettyCode RecursiveBehaviour where
  ppCode :: forall r. Members '[Reader Options] r => RecursiveBehaviour -> Sem r (Doc Ann)
  ppCode (RecursiveBehaviour f m0) = do
    f' <- ppSCode f
    let m' = vsep (map (PP.list . map pretty) m)
    return $
      pretty ("Recursive behaviour of " :: Text) <> f' <> colon <> line
        <> indent 2 (align m')
    where
      m = toList (HashSet.fromList m0)
