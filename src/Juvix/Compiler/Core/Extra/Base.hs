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

mkMatch :: Info -> NonEmpty Node -> [MatchBranch] -> Node
mkMatch i vs bs = NMatch (Match i vs bs)

mkMatch' :: NonEmpty Node -> [MatchBranch] -> Node
mkMatch' = mkMatch Info.empty

mkIf :: Info -> Node -> Node -> Node -> Node
mkIf i v b1 b2 = mkCase i v [CaseBranch Info.empty (BuiltinTag TagTrue) 0 b1] (Just b2)

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

mkTypePrim :: Info -> Primitive -> Type
mkTypePrim i p = NPrim (TypePrim i p)

mkTypePrim' :: Primitive -> Type
mkTypePrim' = mkTypePrim Info.empty

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
mkApps m = foldl' (\acc (i, n) -> mkApp i acc n) m

mkApps' :: Node -> [Node] -> Node
mkApps' n = foldl' mkApp' n

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
mkLambdas is n = foldl' (flip mkLambda) n (reverse is)

mkLambdas' :: Int -> Node -> Node
mkLambdas' k
  | k < 0 = impossible
  | otherwise = mkLambdas (replicate k Info.empty)

unfoldLambdasRev :: Node -> ([Info], Node)
unfoldLambdasRev = go []
  where
    go :: [Info] -> Node -> ([Info], Node)
    go acc n = case n of
      NLam (Lambda i b) -> go (i : acc) b
      _ -> (acc, n)

unfoldLambdas :: Node -> ([Info], Node)
unfoldLambdas = first reverse . unfoldLambdasRev

unfoldLambdas' :: Node -> (Int, Node)
unfoldLambdas' = first length . unfoldLambdas

{------------------------------------------------------------------------}
{- functions on Pattern -}

getBinderPatternInfos :: Pattern -> [Info]
getBinderPatternInfos = go []
  where
    go :: [Info] -> Pattern -> [Info]
    go acc = \case
      PatConstr (PatternConstr {..}) ->
        foldl' go acc _patternConstrArgs
      PatBinder (PatternBinder {..}) ->
        go (_patternBinderInfo : acc) _patternBinderPattern
      PatWildcard {} ->
        acc

getPatternInfos :: Pattern -> [Info]
getPatternInfos = go []
  where
    go :: [Info] -> Pattern -> [Info]
    go acc = \case
      PatConstr (PatternConstr {..}) ->
        foldl' go (_patternConstrInfo : acc) _patternConstrArgs
      PatBinder (PatternBinder {..}) ->
        go (_patternBinderInfo : acc) _patternBinderPattern
      PatWildcard (PatternWildcard {..}) ->
        _patternWildcardInfo : acc

{------------------------------------------------------------------------}
{- generic Node destruction -}

-- | `NodeDetails` is a convenience datatype which provides the most commonly needed
-- information about a node in a generic fashion.
data NodeDetails = NodeDetails
  { -- | 'nodeInfo' is the info associated with the node,
    _nodeInfo :: Info,
    -- | 'nodeSubinfos' contains, in a fixed order, infos other than the main
    -- one, e.g., for a Case these are the infos associated with the branches,
    -- for a Match the infos associated with all patterns in all branches.
    _nodeSubinfos :: [Info],
    -- | 'nodeChildren' are the children, in a fixed order, i.e., the immediate
    -- recursive subnodes
    _nodeChildren :: [Node],
    -- | 'nodeChildBindersNum' is the number of binders introduced for each
    -- child in the parent node. Same length and order as in `nodeChildren`.
    _nodeChildBindersNum :: [Int],
    -- | 'nodeChildBindersInfo' is information about binders for each child, if
    -- present. Same length and order as in `nodeChildren`.
    _nodeChildBindersInfo :: [[Info]],
    -- | 'nodeReassemble' reassembles the node from the info, the subinfos and
    -- the children (which should be in the same fixed order as in the
    -- 'nodeSubinfos' and 'nodeChildren' component).
    _nodeReassemble :: Info -> [Info] -> [Node] -> Node
  }

makeLenses ''NodeDetails

-- | Destruct a node into NodeDetails. This is an ugly internal function used to
-- implement more high-level accessors and recursors.
destruct :: Node -> NodeDetails
destruct = \case
  NVar (Var i idx) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ -> mkVar i' idx
      }
  NIdt (Ident i sym) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ -> mkIdent i' sym
      }
  NCst (Constant i c) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ -> mkConstant i' c
      }
  NApp (App i l r) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [l, r],
        _nodeChildBindersNum = [0, 0],
        _nodeChildBindersInfo = [[], []],
        _nodeReassemble = \i' _ args' -> mkApp i' (hd args') (args' !! 1)
      }
  NBlt (BuiltinApp i op args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = args,
        _nodeChildBindersNum = map (const 0) args,
        _nodeChildBindersInfo = map (const []) args,
        _nodeReassemble = \i' _ args' -> mkBuiltinApp i' op args'
      }
  NCtr (Constr i tag args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = args,
        _nodeChildBindersNum = map (const 0) args,
        _nodeChildBindersInfo = map (const []) args,
        _nodeReassemble = \i' _ args' -> mkConstr i' tag args'
      }
  NLam (Lambda i b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [b],
        _nodeChildBindersNum = [1],
        _nodeChildBindersInfo = [fetchBinderInfo i],
        _nodeReassemble = \i' _ args' -> mkLambda i' (hd args')
      }
  NLet (Let i v b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [v, b],
        _nodeChildBindersNum = [0, 1],
        _nodeChildBindersInfo = [[], fetchBinderInfo i],
        _nodeReassemble = \i' _ args' -> mkLet i' (hd args') (args' !! 1)
      }
  NRec (LetRec i vs b) ->
    let n = length vs
     in NodeDetails
          { _nodeInfo = i,
            _nodeSubinfos = [],
            _nodeChildren = b : toList vs,
            _nodeChildBindersNum = replicate (n + 1) n,
            _nodeChildBindersInfo = replicate (n + 1) (getInfoBinders n i),
            _nodeReassemble = \i' _ args' -> mkLetRec i' (fromList (tl args')) (hd args')
          }
  NCase (Case i v bs Nothing) ->
    let branchBinderNums = map (^. caseBranchBindersNum) bs
        branchBinderInfos = map (\(CaseBranch {..}) -> getInfoBinders _caseBranchBindersNum _caseBranchInfo) bs
     in NodeDetails
          { _nodeInfo = i,
            _nodeSubinfos = map (^. caseBranchInfo) bs,
            _nodeChildren = v : map (^. caseBranchBody) bs,
            _nodeChildBindersNum = 0 : branchBinderNums,
            _nodeChildBindersInfo = [] : branchBinderInfos,
            _nodeReassemble = \i' is' args' ->
              mkCase
                i'
                (hd args')
                ( zipWith3Exact
                    ( \br is body' ->
                        br
                          { _caseBranchInfo = is,
                            _caseBranchBody = body'
                          }
                    )
                    bs
                    is'
                    (tl args')
                )
                Nothing
          }
  NCase (Case i v bs (Just def)) ->
    let branchBinderNums = map (^. caseBranchBindersNum) bs
        branchBinderInfos = map (\(CaseBranch {..}) -> getInfoBinders _caseBranchBindersNum _caseBranchInfo) bs
     in NodeDetails
          { _nodeInfo = i,
            _nodeSubinfos = map (^. caseBranchInfo) bs,
            _nodeChildren = v : def : map (^. caseBranchBody) bs,
            _nodeChildBindersNum = 0 : 0 : branchBinderNums,
            _nodeChildBindersInfo = [] : [] : branchBinderInfos,
            _nodeReassemble = \i' is' args' ->
              mkCase
                i'
                (hd args')
                ( zipWith3Exact
                    ( \br is body' ->
                        br
                          { _caseBranchInfo = is,
                            _caseBranchBody = body'
                          }
                    )
                    bs
                    is'
                    (tl (tl args'))
                )
                (Just (hd (tl args')))
          }
  NMatch (Match i vs bs) ->
    let branchBinderInfos =
          map
            ( \br ->
                concatMap
                  getBinderPatternInfos
                  (reverse (toList (br ^. matchBranchPatterns)))
            )
            bs
        branchBinderNums = map length branchBinderInfos
        branchPatternInfos =
          concatMap
            ( \br ->
                concatMap
                  (reverse . getPatternInfos)
                  (br ^. matchBranchPatterns)
            )
            bs
        n = length vs
     in NodeDetails
          { _nodeInfo = i,
            _nodeSubinfos = branchPatternInfos,
            _nodeChildren = toList vs ++ map (^. matchBranchBody) bs,
            _nodeChildBindersNum = replicate n 0 ++ branchBinderNums,
            _nodeChildBindersInfo = replicate n [] ++ branchBinderInfos,
            _nodeReassemble = \i' is' args' ->
              mkMatch
                i'
                (fromList $ List.take n args')
                ( zipWithExact
                    ( \br body' ->
                        br
                          { _matchBranchPatterns =
                              fromList $ setPatternsInfos is' (toList (br ^. matchBranchPatterns)),
                            _matchBranchBody = body'
                          }
                    )
                    bs
                    (drop n args')
                )
          }
  NPi (Pi i ty b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [ty, b],
        _nodeChildBindersNum = [0, 1],
        _nodeChildBindersInfo = [[], fetchBinderInfo i],
        _nodeReassemble = \i' _ args' -> mkPi i' (hd args') (args' !! 1)
      }
  NUniv (Univ i l) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ -> mkUniv i' l
      }
  NTyp (TypeConstr i sym args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = args,
        _nodeChildBindersNum = map (const 0) args,
        _nodeChildBindersInfo = map (const []) args,
        _nodeReassemble = \i' _ args' -> mkTypeConstr i' sym args'
      }
  NPrim (TypePrim i prim) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ -> mkTypePrim i' prim
      }
  NDyn (Dynamic i) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ -> mkDynamic i'
      }
  Closure env (Lambda i b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = b : env,
        _nodeChildBindersNum = 1 : map (const 0) env,
        _nodeChildBindersInfo = fetchBinderInfo i : map (const []) env,
        _nodeReassemble = \i' _ args' -> Closure (tl args') (Lambda i' (hd args'))
      }
  where
    fetchBinderInfo :: Info -> [Info]
    fetchBinderInfo i = [getInfoBinder i]

    setPatternsInfos :: [Info] -> [Pattern] -> [Pattern]
    setPatternsInfos is ps = snd $ setPatternsInfos' is ps

    setPatternsInfos' :: [Info] -> [Pattern] -> ([Info], [Pattern])
    setPatternsInfos' is [] = (is, [])
    setPatternsInfos' is (p : ps) =
      let (is', p') = setPatInfos is p
          (is'', ps') = setPatternsInfos' is' ps
       in (is'', p' : ps')

    setPatInfos :: [Info] -> Pattern -> ([Info], Pattern)
    setPatInfos is = \case
      PatWildcard x ->
        (tl is, PatWildcard (x {_patternWildcardInfo = hd is}))
      PatBinder x ->
        (tl is, PatBinder (x {_patternBinderInfo = hd is}))
      PatConstr x ->
        let (is', ps) = setPatternsInfos' (tl is) (x ^. patternConstrArgs)
         in (is', PatConstr (x {_patternConstrInfo = hd is, _patternConstrArgs = ps}))

    hd :: [a] -> a
    hd = List.head

    tl :: [a] -> [a]
    tl = List.tail

reassemble :: Node -> [Node] -> Node
reassemble n = (d ^. nodeReassemble) (d ^. nodeInfo) (d ^. nodeSubinfos)
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

modifyInfoM :: Monad m => (Info -> m Info) -> Node -> m Node
modifyInfoM f n =
  let ni = destruct n
   in do
        i' <- f (ni ^. nodeInfo)
        is' <- mapM f (ni ^. nodeSubinfos)
        return ((ni ^. nodeReassemble) i' is' (ni ^. nodeChildren))

modifyInfo :: (Info -> Info) -> Node -> Node
modifyInfo f n = runIdentity $ modifyInfoM (pure . f) n
