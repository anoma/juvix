module Juvix.Compiler.Core.Extra.Base where

import Data.Functor.Identity
import Data.List qualified as List
import Data.List.NonEmpty (fromList)
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.BinderInfo
import Juvix.Compiler.Core.Language

{------------------------------------------------------------------------}
{- Node constructors -}

mkVar :: Info -> Index -> Node
mkVar i idx = NVar (Var i idx)

mkVar' :: Index -> Node
mkVar' = mkVar Info.empty

mkIdent :: Info -> Symbol -> Node
mkIdent i sym = NIdt (Ident i sym)

mkIdent' :: Symbol -> Node
mkIdent' = mkIdent Info.empty

mkConstant :: Info -> ConstantValue -> Node
mkConstant i cv = NCst (Constant i cv)

mkConstant' :: ConstantValue -> Node
mkConstant' = mkConstant Info.empty

mkApp :: Info -> Node -> Node -> Node
mkApp i l r = NApp (App i l r)

mkApp' :: Node -> Node -> Node
mkApp' = mkApp Info.empty

mkBuiltinApp :: Info -> BuiltinOp -> [Node] -> Node
mkBuiltinApp i op args = NBlt (BuiltinApp i op args)

mkBuiltinApp' :: BuiltinOp -> [Node] -> Node
mkBuiltinApp' = mkBuiltinApp Info.empty

mkConstr :: Info -> Tag -> [Node] -> Node
mkConstr i tag args = NCtr (Constr i tag args)

mkConstr' :: Tag -> [Node] -> Node
mkConstr' = mkConstr Info.empty

mkLambda :: Info -> Node -> Node
mkLambda i b = NLam (Lambda i b)

mkLambda' :: Node -> Node
mkLambda' = mkLambda Info.empty

mkLet :: Info -> Node -> Node -> Node
mkLet i v b = NLet (Let i v b)

mkLet' :: Node -> Node -> Node
mkLet' = mkLet Info.empty

mkLetRec :: Info -> NonEmpty Node -> Node -> Node
mkLetRec i vs b = NRec (LetRec i vs b)

mkLetRec' :: NonEmpty Node -> Node -> Node
mkLetRec' = mkLetRec Info.empty

mkCase :: Info -> Node -> [CaseBranch] -> Maybe Node -> Node
mkCase i v bs def = NCase (Case i v bs def)

mkCase' :: Node -> [CaseBranch] -> Maybe Node -> Node
mkCase' = mkCase Info.empty

mkIf :: Info -> Node -> Node -> Node -> Node
mkIf i v b1 b2 = mkCase i v [CaseBranch (BuiltinTag TagTrue) 0 b1] (Just b2)

mkIf' :: Node -> Node -> Node -> Node
mkIf' = mkIf Info.empty

{------------------------------------------------------------------------}
{- Type constructors -}

mkPi :: Info -> Type -> Type -> Type
mkPi i ty b = NPi (Pi i ty b)

mkPi' :: Type -> Type -> Type
mkPi' = mkPi Info.empty

mkUniv :: Info -> Int -> Type
mkUniv i l = NUniv (Univ i l)

mkUniv' :: Int -> Type
mkUniv' = mkUniv Info.empty

mkTypeConstr :: Info -> Symbol -> [Type] -> Type
mkTypeConstr i sym args = NTyp (TypeConstr i sym args)

mkTypeConstr' :: Symbol -> [Type] -> Type
mkTypeConstr' = mkTypeConstr Info.empty

mkDynamic :: Info -> Type
mkDynamic i = NDyn (Dynamic i)

mkDynamic' :: Type
mkDynamic' = mkDynamic Info.empty

{------------------------------------------------------------------------}
{- functions on Type -}

-- | Unfold a type into the target and the arguments (left-to-right)
unfoldType' :: Type -> (Type, [(Info, Type)])
unfoldType' ty = case ty of
  NPi (Pi i l r) -> let (tgt, args) = unfoldType' r in (tgt, (i, l) : args)
  _ -> (ty, [])

{------------------------------------------------------------------------}
{- functions on Node -}

mkApps :: Node -> [(Info, Node)] -> Node
mkApps = foldl' (\acc (i, n) -> mkApp i acc n)

mkApps' :: Node -> [Node] -> Node
mkApps' = foldl' mkApp'

unfoldApps :: Node -> (Node, [(Info, Node)])
unfoldApps = go []
  where
    go :: [(Info, Node)] -> Node -> (Node, [(Info, Node)])
    go acc n = case n of
      NApp (App i l r) -> go ((i, r) : acc) l
      _ -> (n, acc)

unfoldApps' :: Node -> (Node, [Node])
unfoldApps' = second (map snd) . unfoldApps

mkLambdas :: [Info] -> Node -> Node
mkLambdas is n = foldl' (flip mkLambda) n is

mkLambdas' :: Int -> Node -> Node
mkLambdas' k
  | k < 0 = impossible
  | otherwise = mkLambdas (replicate k Info.empty)

unfoldLambdas :: Node -> ([Info], Node)
unfoldLambdas = go []
  where
    go :: [Info] -> Node -> ([Info], Node)
    go acc n = case n of
      NLam (Lambda i b) -> go (i : acc) b
      _ -> (acc, n)

unfoldLambdas' :: Node -> (Int, Node)
unfoldLambdas' = first length . unfoldLambdas

-- | `NodeDetails` is a convenience datatype which provides the most commonly needed
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
    _nodeChildBindersInfo :: [[Info]],
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
  NVar (Var i idx) -> NodeDetails i [] [] [] (\i' _ -> mkVar i' idx)
  NIdt (Ident i sym) -> NodeDetails i [] [] [] (\i' _ -> mkIdent i' sym)
  NCst (Constant i c) -> NodeDetails i [] [] [] (\i' _ -> mkConstant i' c)
  NApp (App i l r) -> NodeDetails i [l, r] [0, 0] [[], []] (\i' args' -> mkApp i' (hd args') (args' !! 1))
  NBlt (BuiltinApp i op args) -> NodeDetails i args (map (const 0) args) (map (const []) args) (`mkBuiltinApp` op)
  NCtr (Constr i tag args) -> NodeDetails i args (map (const 0) args) (map (const []) args) (`mkConstr` tag)
  NLam (Lambda i b) -> NodeDetails i [b] [1] [fetchBinderInfo i] (\i' args' -> mkLambda i' (hd args'))
  NLet (Let i v b) -> NodeDetails i [v, b] [0, 1] [[], fetchBinderInfo i] (\i' args' -> mkLet i' (hd args') (args' !! 1))
  NRec (LetRec i vs b) ->
    let n = length vs
     in NodeDetails
          i
          (b : toList vs)
          (replicate (n + 1) n)
          (replicate (n + 1) (getInfoBinders n i))
          (\i' args' -> mkLetRec i' (fromList (tl args')) (hd args'))
  NCase (Case i v bs Nothing) ->
    let branchBinderNums = map (\(CaseBranch _ k _) -> k) bs
     in NodeDetails
          i
          (v : map (\(CaseBranch _ _ br) -> br) bs)
          (0 : branchBinderNums)
          ([] : fetchCaseBinderInfo i (map (`replicate` Info.empty) branchBinderNums))
          ( \i' args' ->
              mkCase
                i'
                (hd args')
                ( zipWithExact
                    (\(CaseBranch tag k _) br' -> CaseBranch tag k br')
                    bs
                    (tl args')
                )
                Nothing
          )
  NCase (Case i v bs (Just def)) ->
    let branchBinderNums = map (\(CaseBranch _ k _) -> k) bs
     in NodeDetails
          i
          (v : def : map (\(CaseBranch _ _ br) -> br) bs)
          (0 : 0 : branchBinderNums)
          ([] : [] : fetchCaseBinderInfo i (map (`replicate` Info.empty) branchBinderNums))
          ( \i' args' ->
              mkCase
                i'
                (hd args')
                ( zipWithExact
                    (\(CaseBranch tag k _) br' -> CaseBranch tag k br')
                    bs
                    (tl (tl args'))
                )
                (Just (hd (tl args')))
          )
  NPi (Pi i ty b) ->
    NodeDetails i [ty, b] [0, 1] [[], fetchBinderInfo i] (\i' args' -> mkPi i' (hd args') (args' !! 1))
  NUniv (Univ i l) ->
    NodeDetails i [] [] [] (\i' _ -> mkUniv i' l)
  NTyp (TypeConstr i sym args) ->
    NodeDetails i args (map (const 0) args) (map (const []) args) (`mkTypeConstr` sym)
  NDyn (Dynamic i) ->
    NodeDetails i [] [] [] (\i' _ -> mkDynamic i')
  Closure env (Lambda i b) ->
    NodeDetails
      i
      (b : env)
      (1 : map (const 0) env)
      (fetchBinderInfo i : map (const []) env)
      (\i' args' -> Closure (tl args') (Lambda i' (hd args')))
  where
    fetchBinderInfo :: Info -> [Info]
    fetchBinderInfo i = [getInfoBinder i]

    fetchCaseBinderInfo :: Info -> [[Info]] -> [[Info]]
    fetchCaseBinderInfo i d = case Info.lookup kCaseBinderInfo i of
      Just cbi -> cbi ^. infoBranchBinders
      Nothing -> d

    hd :: [a] -> a
    hd = List.head

    tl :: [a] -> [a]
    tl = List.tail

reassemble :: Node -> [Node] -> Node
reassemble n = (d ^. nodeReassemble) (d ^. nodeInfo)
  where
    d = destruct n

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
