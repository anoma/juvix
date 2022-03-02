{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Termination.CallGraph.Types (
  module MiniJuvix.Termination.CallGraph.Types.SizeRelation,
  module MiniJuvix.Termination.CallGraph.Types
                                             ) where

import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Abstract.Language as A
import qualified Data.HashMap.Strict as HashMap
import Prettyprinter
import MiniJuvix.Termination.CallGraph.Types.SizeRelation
import MiniJuvix.Syntax.Abstract.Pretty.Base

newtype CallMap = CallMap {
  _callGraph :: HashMap A.FunctionName (HashMap A.FunctionName [FunCall]) }
  deriving newtype (Semigroup, Monoid)

data Argument = Argument {
  _argOwnerFunction :: A.FunctionName,
  _argIx :: Int
  }
data FunCall = FunCall {
  _callName :: A.FunctionName,
  _callArgs :: [(ArgRelation, A.Expression)]
  }

data ArgRelation =
  LessThan Int
  | EqualTo Int
  | DontKnow

newtype CallRow = CallRow {
  _callRow :: Maybe (Int, Rel')
  }

type CallMatrix = [CallRow]
data Call = Call {
  _callFrom :: A.FunctionName,
  _callTo :: A.FunctionName,
  _callMatrix :: CallMatrix
  }
type Edges = HashMap (A.FunctionName, A.FunctionName) Edge

data Edge = Edge {
  _edgeFrom :: A.FunctionName,
  _edgeTo :: A.FunctionName,
  _edgeMatrices :: [CallMatrix]
  }

newtype CompleteCallGraph = CompleteCallGraph Edges

data ReflexiveEdge = ReflexiveEdge {
  _redgeMatrix :: CallMatrix,
  _redgeFun :: A.FunctionName
  }

makeLenses ''FunCall
makeLenses ''Edge
makeLenses ''CallMap

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

fromFunCall :: A.FunctionName -> FunCall -> Call
fromFunCall caller fc =
  Call {_callFrom = caller,
          _callTo = fc ^. callName,
          _callMatrix = map (fromArgRelation . fst) (fc ^. callArgs)
       }
  where
  fromArgRelation :: ArgRelation -> CallRow
  fromArgRelation a = CallRow $ case a of
    DontKnow -> Nothing
    LessThan i -> Just (i, RLe)
    EqualTo i -> Just (i, REq)

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
               | (caller, callerMap) <- HashMap.toList (cm ^. callGraph),
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


instance PrettyCode FunCall where
  ppCode :: forall r. Members '[Reader Options] r => FunCall -> Sem r (Doc Ann)
  ppCode (FunCall f args) = do
    args' <- mapM ppArg args
    f' <- ppSCode f
    return $ f' <+> hsep args'
    where
    ppArg :: (ArgRelation, A.Expression) -> Sem r (Doc Ann)
    ppArg (mi, a) =
      case mi of
        LessThan i -> relAux kwPred (Just i)
        EqualTo i -> relAux kwEqual (Just i)
        DontKnow -> relAux kwQuestion Nothing
      where
      relAux kwRel mayIx = do
        showDecr <- asks _optShowDecreasingArgs
        let pi = annotate AnnImportant . pretty <$> mayIx
        case showDecr of
          OnlyArg -> ppCodeAtom a
          OnlyRel -> return $ dbrackets (kwRel <+?> pi)
          ArgRel -> do
            a' <- ppCode a
            return $ dbrackets (a' <+> kwRel <+?> pi)
    dbrackets :: Doc a -> Doc a
    dbrackets x = pretty '⟦' <> x <> pretty '⟧'
    kwPred :: Doc Ann
    kwPred = annotate AnnKeyword "≺"
    kwEqual :: Doc Ann
    kwEqual = annotate AnnKeyword "∼"

instance PrettyCode CallMap where
  ppCode :: forall r. Members '[Reader Options] r => CallMap -> Sem r (Doc Ann)
  ppCode (CallMap m) = vsep <$> mapM ppEntry (HashMap.toList m)
    where
    ppEntry :: (A.FunctionName, HashMap A.FunctionName [FunCall]) -> Sem r (Doc Ann)
    ppEntry (fun, mcalls) = do
      fun' <- annotate AnnImportant <$> ppSCode fun
      calls' <- vsep <$> mapM ppCode calls
      return $ fun' <+> waveFun <+> align calls'
      where
      calls = concat (HashMap.elems mcalls)

kwQuestion :: Doc Ann
kwQuestion = annotate AnnKeyword "?"

waveFun :: Doc ann
waveFun = pretty ("↝" :: Text)

vsep2 :: [Doc ann] -> Doc ann
vsep2 = concatWith (\a b -> a <> line <> line <> b)

instance PrettyCode CallRow where
  ppCode (CallRow r) = return $ case r of
    Nothing -> kwQuestion
    Just (i, r') ->
      pretty i <+> annotate AnnKeyword (pretty r')

instance PrettyCode CallMatrix where
  ppCode l = vsep <$> mapM ppCode l

instance PrettyCode Edge where
  ppCode Edge {..} = do
    fromFun <- ppSCode _edgeFrom
    toFun <- ppSCode _edgeTo
    matrices <- ppMatrices . zip [0 :: Int ..] <$> mapM ppCode _edgeMatrices
    return $ pretty ("Edge" :: Text) <+> fromFun <+> waveFun <+> toFun <> line
      <> matrices
    where
    ppMatrices = vsep2 . map ppMatrix
    ppMatrix (i, t) = pretty ("Matrix" :: Text) <+> pretty i <> colon <> line
      <> t

instance PrettyCode CompleteCallGraph where
  ppCode :: forall r. Members '[Reader Options] r => CompleteCallGraph -> Sem r (Doc Ann)
  ppCode (CompleteCallGraph edges) = do
    es <- vsep2 <$> mapM ppCode (toList edges)
    return $ pretty ("Complete Call Graph" :: Text) <> line <> es
