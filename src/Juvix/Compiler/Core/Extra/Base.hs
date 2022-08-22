module Juvix.Compiler.Core.Extra.Base where

import Data.Functor.Identity
import Data.List qualified as List
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Info qualified as Info
import Juvix.Compiler.Core.Language.Info.BinderInfo

{------------------------------------------------------------------------}
{- functions on Type -}

-- unfold a type into the target and the arguments (left-to-right)
unfoldType' :: Type -> (Type, [(Info, Type)])
unfoldType' ty = case ty of
  Pi i l r -> let (tgt, args) = unfoldType' r in (tgt, (i, l) : args)
  _ -> (ty, [])

{------------------------------------------------------------------------}
{- functions on Node -}

mkApp' :: Node -> [(Info, Node)] -> Node
mkApp' = foldl' (\acc (i, n) -> App i acc n)

mkApp :: Node -> [Node] -> Node
mkApp = foldl' (App Info.empty)

unfoldApp' :: Node -> (Node, [(Info, Node)])
unfoldApp' = go []
  where
    go :: [(Info, Node)] -> Node -> (Node, [(Info, Node)])
    go acc n = case n of
      App i l r -> go ((i, r) : acc) l
      _ -> (n, acc)

unfoldApp :: Node -> (Node, [Node])
unfoldApp = second (map snd) . unfoldApp'

mkLambdas' :: [Info] -> Node -> Node
mkLambdas' is n = foldr Lambda n is

mkLambdas :: Int -> Node -> Node
mkLambdas k = mkLambdas' (replicate k Info.empty)

unfoldLambdas' :: Node -> ([Info], Node)
unfoldLambdas' = go []
  where
    go :: [Info] -> Node -> ([Info], Node)
    go acc n = case n of
      Lambda i b -> go (i : acc) b
      _ -> (acc, n)

unfoldLambdas :: Node -> (Int, Node)
unfoldLambdas = first length . unfoldLambdas'

-- `NodeDetails` is a convenience datatype which provides the most commonly needed
-- information about a node in a generic fashion.
data NodeDetails = NodeDetails
  { -- `nodeInfo` is the info associated with the node,
    _nodeInfo :: Info,
    -- `nodeChildren` are the children, in a fixed order, i.e., the immediate
    -- recursive subnodes
    _nodeChildren :: [Node],
    -- `nodeChildBindersNum` is the number of binders introduced for each child
    -- in the parent node. Same length and order as in `nodeChildren`.
    _nodeChildBindersNum :: [Int],
    -- `nodeChildBindersInfo` is information about binders for each child, if
    -- present. Same length and order as in `nodeChildren`.
    _nodeChildBindersInfo :: [Maybe [BinderInfo]],
    -- `nodeReassemble` reassembles the node from the info and the children
    -- (which should be in the same fixed order as in the `nodeChildren`
    -- component).
    _nodeReassemble :: Info -> [Node] -> Node
  }

makeLenses ''NodeDetails

-- Destruct a node into NodeDetails. This is an ugly internal function used to
-- implement more high-level accessors and recursors.
destruct :: Node -> NodeDetails
destruct = \case
  Var i idx -> NodeDetails i [] [] [] (\i' _ -> Var i' idx)
  Ident i sym -> NodeDetails i [] [] [] (\i' _ -> Ident i' sym)
  Constant i c -> NodeDetails i [] [] [] (\i' _ -> Constant i' c)
  App i l r -> NodeDetails i [l, r] [0, 0] [Nothing, Nothing] (\i' args' -> App i' (hd args') (args' !! 1))
  BuiltinApp i op args -> NodeDetails i args (map (const 0) args) (map (const Nothing) args) (`BuiltinApp` op)
  Constr i tag args -> NodeDetails i args (map (const 0) args) (map (const Nothing) args) (`Constr` tag)
  Lambda i b -> NodeDetails i [b] [1] [fetchBinderInfo i] (\i' args' -> Lambda i' (hd args'))
  Let i v b -> NodeDetails i [v, b] [0, 1] [Nothing, fetchBinderInfo i] (\i' args' -> Let i' (hd args') (args' !! 1))
  Case i v bs Nothing ->
    NodeDetails
      i
      (v : map (\(CaseBranch _ _ br) -> br) bs)
      (0 : map (\(CaseBranch _ k _) -> k) bs)
      (Nothing : fetchCaseBinderInfo i (replicate (length bs) Nothing))
      ( \i' args' ->
          Case
            i'
            (hd args')
            ( zipWithExact
                (\(CaseBranch tag k _) br' -> CaseBranch tag k br')
                bs
                (tl args')
            )
            Nothing
      )
  Case i v bs (Just def) ->
    NodeDetails
      i
      (v : def : map (\(CaseBranch _ _ br) -> br) bs)
      (0 : 0 : map (\(CaseBranch _ k _) -> k) bs)
      (Nothing : Nothing : fetchCaseBinderInfo i (replicate (length bs) Nothing))
      ( \i' args' ->
          Case
            i'
            (hd args')
            ( zipWithExact
                (\(CaseBranch tag k _) br' -> CaseBranch tag k br')
                bs
                (tl (tl args'))
            )
            (Just (hd (tl args')))
      )
  If i v b1 b2 ->
    NodeDetails
      i
      [v, b1, b2]
      [0, 0, 0]
      [Nothing, Nothing, Nothing]
      (\i' args' -> If i' (hd args') (args' !! 1) (args' !! 2))
  Pi i ty b ->
    NodeDetails i [ty, b] [0, 1] [Nothing, fetchBinderInfo i] (\i' args' -> Pi i' (hd args') (args' !! 1))
  Univ i l ->
    NodeDetails i [] [] [] (\i' _ -> Univ i' l)
  TypeApp i sym args ->
    NodeDetails i args (map (const 0) args) (map (const Nothing) args) (`TypeApp` sym)
  Closure i env b ->
    NodeDetails
      i
      (b : env)
      (1 : map (const 0) env)
      (fetchBinderInfo i : map (const Nothing) env)
      (\i' args' -> Closure i' (tl args') (hd args'))
  where
    fetchBinderInfo :: Info -> Maybe [BinderInfo]
    fetchBinderInfo i = case Info.lookup kBinderInfo i of
      Just bi -> Just [bi]
      Nothing -> Nothing

    fetchCaseBinderInfo :: Info -> [Maybe [BinderInfo]] -> [Maybe [BinderInfo]]
    fetchCaseBinderInfo i d = case Info.lookup kCaseBinderInfo i of
      Just cbi -> map Just (cbi ^. infoBranchBinders)
      Nothing -> d

    hd :: [a] -> a
    hd = List.head

    tl :: [a] -> [a]
    tl = List.tail

children :: Node -> [Node]
children = (^. nodeChildren) . destruct

-- children together with the number of binders
bchildren :: Node -> [(Int, Node)]
bchildren n =
  let ni = destruct n
   in zipExact (ni ^. nodeChildBindersNum) (ni ^. nodeChildren)

-- shallow children: not under binders
schildren :: Node -> [Node]
schildren = map snd . filter (\p -> fst p == 0) . bchildren

getInfo :: Node -> Info
getInfo = (^. nodeInfo) . destruct

modifyInfoM :: Applicative m => (Info -> m Info) -> Node -> m Node
modifyInfoM f n =
  let ni = destruct n
   in do
        i' <- f (ni ^. nodeInfo)
        return ((ni ^. nodeReassemble) i' (ni ^. nodeChildren))

modifyInfo :: (Info -> Info) -> Node -> Node
modifyInfo f n = runIdentity $ modifyInfoM (pure . f) n
