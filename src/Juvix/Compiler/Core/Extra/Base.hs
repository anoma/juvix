module Juvix.Compiler.Core.Extra.Base where

import Data.Functor.Identity
import Data.List qualified as List
import Juvix.Compiler.Core.Info qualified as Info
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

-- TODO remove
mkLambdaOld :: Info -> Node -> Node
mkLambdaOld i b = NLam (Lambda i emptyBinder b)

mkLambda :: Info -> Binder -> Node -> Node
mkLambda i bi b = NLam (Lambda i bi b)

mkLambda' :: Node -> Node
mkLambda' = mkLambda Info.empty emptyBinder

mkLetItem' :: Node -> LetItem
mkLetItem' = LetItem emptyBinder

mkLet :: Info -> Binder -> Node -> Node -> Node
mkLet i bi v b = NLet (Let i (LetItem bi v) b)

mkLet' :: Node -> Node -> Node
mkLet' = mkLet Info.empty emptyBinder

mkLetRec :: Info -> NonEmpty LetItem -> Node -> Node
mkLetRec i vs b = NRec (LetRec i vs b)

mkLetRec' :: NonEmpty Node -> Node -> Node
mkLetRec' = mkLetRec Info.empty . fmap mkLetItem'

mkCase :: Info -> Node -> [CaseBranch] -> Maybe Node -> Node
mkCase i v bs def = NCase (Case i v bs def)

mkCase' :: Node -> [CaseBranch] -> Maybe Node -> Node
mkCase' = mkCase Info.empty

mkMatch :: Info -> NonEmpty Node -> [MatchBranch] -> Node
mkMatch i vs bs = NMatch (Match i vs bs)

mkMatch' :: NonEmpty Node -> [MatchBranch] -> Node
mkMatch' = mkMatch Info.empty

mkIf :: Info -> Node -> Node -> Node -> Node
mkIf i v b1 b2 = mkCase i v [br] (Just b2)
  where
    br =
      CaseBranch
        { _caseBranchInfo = mempty,
          _caseBranchTag = BuiltinTag TagTrue,
          _caseBranchBinders = [],
          _caseBranchBindersNum = 0,
          _caseBranchBody = b1
        }

mkIf' :: Node -> Node -> Node -> Node
mkIf' = mkIf Info.empty

{------------------------------------------------------------------------}
{- Type constructors -}

mkPi :: Info -> Binder -> Type -> Type
mkPi i bi b = NPi (Pi i bi b)

mkPi' :: Type -> Type -> Type
mkPi' = mkPi Info.empty . Binder Nothing

mkPis :: [Binder] -> Type -> Type
mkPis tys ty = foldr (mkPi mempty) ty tys

rePi :: PiLhs -> Type -> Type
rePi PiLhs {..} = mkPi _piLhsInfo _piLhsBinder

rePis :: [PiLhs] -> Type -> Type
rePis tys ty = foldr rePi ty tys

mkPis' :: [Type] -> Type -> Type
mkPis' tys ty = foldr mkPi' ty tys

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

mkTypeInteger :: Info -> Type
mkTypeInteger i = mkTypePrim i (PrimInteger (PrimIntegerInfo Nothing Nothing))

mkTypeInteger' :: Type
mkTypeInteger' = mkTypeInteger Info.empty

mkTypeBool :: Info -> Type
mkTypeBool i = mkTypePrim i (PrimBool (PrimBoolInfo (BuiltinTag TagTrue) (BuiltinTag TagFalse)))

mkTypeBool' :: Type
mkTypeBool' = mkTypeBool Info.empty

mkTypeString :: Info -> Type
mkTypeString i = mkTypePrim i PrimString

mkTypeString' :: Type
mkTypeString' = mkTypeString Info.empty

mkDynamic :: Info -> Type
mkDynamic i = NDyn (Dynamic i)

mkDynamic' :: Type
mkDynamic' = mkDynamic Info.empty

{------------------------------------------------------------------------}
{- functions on Type -}

-- | Unfold a type into the target and the arguments (left-to-right)
unfoldPi :: Type -> ([PiLhs], Type)
unfoldPi ty = case ty of
  NPi (Pi i bi r) ->
    let (args, target) = unfoldPi r
     in (PiLhs i bi : args, target)
  _ -> ([], ty)

typeArgs :: Type -> [Type]
typeArgs = map (^. piLhsBinder . binderType) . fst . unfoldPi

typeTarget :: Type -> Type
typeTarget = snd . unfoldPi

isDynamic :: Type -> Bool
isDynamic = \case
  NDyn {} -> True
  _ -> False

-- | `expandType argtys ty` expands the dynamic target of `ty` to match the
-- number of arguments with types specified by `argstys`. For example,
-- `expandType [int, string] (int -> any) = int -> string -> any`.
expandType :: [Type] -> Type -> Type
expandType argtys ty =
  let (tyargs, target) = unfoldPi ty
   in if
          | length tyargs >= length argtys ->
              ty
          | isDynamic target ->
              rePis tyargs (mkPis' (drop (length tyargs) argtys) target)
          | otherwise ->
              impossible

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
mkLambdas is n = foldl' (flip mkLambdaOld) n (reverse is)

reLambda :: LambdaLhs -> Node -> Node
reLambda lhs = mkLambda (lhs ^. lambdaLhsInfo) (lhs ^. lambdaLhsBinder)

reLambdas :: [LambdaLhs] -> Node -> Node
reLambdas is n = foldl' (flip reLambda) n (reverse is)

-- | the given info corresponds to the binder info
mkLambdaB :: Binder -> Node -> Node
mkLambdaB = mkLambda mempty

-- | the given infos correspond to the binder infos
mkLambdasB :: [Binder] -> Node -> Node
mkLambdasB is n = foldl' (flip mkLambdaB) n (reverse is)

mkLambdas' :: Int -> Node -> Node
mkLambdas' k
  | k < 0 = impossible
  | otherwise = mkLambdas (replicate k Info.empty)

unfoldLambdasRev :: Node -> ([LambdaLhs], Node)
unfoldLambdasRev = go []
  where
    go :: [LambdaLhs] -> Node -> ([LambdaLhs], Node)
    go acc n = case n of
      NLam (Lambda i bi b) -> go (LambdaLhs i bi : acc) b
      _ -> (acc, n)

unfoldLambdas :: Node -> ([LambdaLhs], Node)
unfoldLambdas = first reverse . unfoldLambdasRev

unfoldLambdas' :: Node -> (Int, Node)
unfoldLambdas' = first length . unfoldLambdas

{------------------------------------------------------------------------}
{- functions on Pattern -}

getBinderPatternInfos :: Pattern -> [Binder]
getBinderPatternInfos = go []
  where
    go :: [Binder] -> Pattern -> [Binder]
    go acc = \case
      PatConstr PatternConstr {..} -> foldl' go acc _patternConstrArgs
      PatBinder p -> go (p ^. patternBinder : acc) (p ^. patternBinderPattern)
      PatWildcard {} -> acc

getPatternInfos :: Pattern -> [Info]
getPatternInfos = go []
  where
    go :: [Info] -> Pattern -> [Info]
    go acc = \case
      PatConstr PatternConstr {..} -> foldl' go (_patternConstrInfo : acc) _patternConstrArgs
      PatBinder PatternBinder {..} -> go acc _patternBinderPattern
      PatWildcard PatternWildcard {..} -> _patternWildcardInfo : acc

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
    _nodeChildBindersInfo :: [[Binder]],
    -- | 'nodeReassemble' reassembles the node from the info, the subinfos and
    -- the children (which should be in the same fixed order as in the
    -- 'nodeSubinfos' and 'nodeChildren' component).
    -- TODO can we zip the lists?
    _nodeReassemble :: Info -> [Info] -> [[Binder]] -> [Node] -> Node
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
        _nodeReassemble = \i' _ _ _ -> mkVar i' idx
      }
  NIdt (Ident i sym) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ _ -> mkIdent i' sym
      }
  NCst (Constant i c) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ _ -> mkConstant i' c
      }
  NApp (App i l r) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [l, r],
        _nodeChildBindersNum = [0, 0],
        _nodeChildBindersInfo = [[], []],
        _nodeReassemble = \i' _ _ args' -> mkApp i' (hd args') (args' !! 1)
      }
  NBlt (BuiltinApp i op args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = args,
        _nodeChildBindersNum = map (const 0) args,
        _nodeChildBindersInfo = map (const []) args,
        _nodeReassemble = \i' _ _ args' -> mkBuiltinApp i' op args'
      }
  NCtr (Constr i tag args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = args,
        _nodeChildBindersNum = map (const 0) args,
        _nodeChildBindersInfo = map (const []) args,
        _nodeReassemble = \i' _ _ args' -> mkConstr i' tag args'
      }
  NLam (Lambda i bi b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [b],
        _nodeChildBindersNum = [1],
        _nodeChildBindersInfo = [[bi]],
        _nodeReassemble = \i' _ binders' args' -> mkLambda i' (hd (hd binders')) (hd args')
      }
  NLet (Let i (LetItem bi v) b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [v, b],
        _nodeChildBindersNum = [0, 1],
        _nodeChildBindersInfo = [[], [bi]],
        _nodeReassemble = \i' _ binders' args' -> mkLet i' (hd (hd binders')) (hd args') (args' !! 1)
      }
  NRec (LetRec i vs b) ->
    let n = length vs
     in NodeDetails
          { _nodeInfo = i,
            _nodeSubinfos = [],
            _nodeChildren = b : map (^. letItemValue) (toList vs),
            _nodeChildBindersNum = replicate (n + 1) n,
            _nodeChildBindersInfo = replicate (n + 1) (map (^. letItemBinder) (toList vs)),
            _nodeReassemble = \i' _ binders' args' ->
              mkLetRec i' (nonEmpty' (zipWithExact LetItem (hd binders') (tl args'))) (hd args')
          }
  NCase (Case i v brs mdef) ->
    let branchBinderNums = map (^. caseBranchBindersNum) brs
        branchBinderInfos :: [[Binder]]
        branchBinderInfos = map (^. caseBranchBinders) brs
     in case mdef of
          Nothing ->
            NodeDetails
              { _nodeInfo = i,
                _nodeSubinfos = map (^. caseBranchInfo) brs,
                _nodeChildren = v : map (^. caseBranchBody) brs,
                _nodeChildBindersNum = 0 : branchBinderNums,
                _nodeChildBindersInfo = [] : branchBinderInfos,
                _nodeReassemble = \i' is' _ args' ->
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
                        brs
                        is'
                        (tl args')
                    )
                    Nothing
              }
          Just def ->
            NodeDetails
              { _nodeInfo = i,
                _nodeSubinfos = map (^. caseBranchInfo) brs,
                _nodeChildren = v : def : map (^. caseBranchBody) brs,
                _nodeChildBindersNum = 0 : 0 : branchBinderNums,
                _nodeChildBindersInfo = [] : [] : branchBinderInfos,
                _nodeReassemble = \i' is' binders' args' ->
                  mkCase
                    i'
                    (hd args')
                    ( zipWith3Exact
                        ( \br (bis, is) body' ->
                            br
                              { _caseBranchInfo = is,
                                _caseBranchBinders = bis,
                                _caseBranchBody = body'
                              }
                        )
                        brs
                        (zipExact binders' is')
                        (tl (tl args'))
                    )
                    (Just (hd (tl args')))
              }
  NMatch (Match i vs branches) ->
    let branchBinders :: [[Binder]]
        branchBinders =
          map
            ( \br ->
                concatMap
                  getBinderPatternInfos
                  (reverse (toList (br ^. matchBranchPatterns)))
            )
            branches
        branchBinderNums = map length branchBinders
        branchPatternInfos :: [Info]
        branchPatternInfos =
          concatMap
            ( \br ->
                concatMap
                  (reverse . getPatternInfos)
                  (br ^. matchBranchPatterns)
            )
            branches
        n = length vs
     in NodeDetails
          { _nodeInfo = i,
            _nodeSubinfos = branchPatternInfos,
            _nodeChildren = toList vs ++ map (^. matchBranchBody) branches,
            _nodeChildBindersNum = replicate n 0 ++ branchBinderNums,
            _nodeChildBindersInfo = replicate n [] ++ branchBinders,
            _nodeReassemble = \i' is' binders' args' ->
              mkMatch
                i'
                (nonEmpty' $ take n args')
                ( zipWithExact
                    ( \br (brbinders', body') ->
                        br
                          { _matchBranchPatterns =
                              nonEmpty' $ setPatternsInfos brbinders' is' (toList (br ^. matchBranchPatterns)),
                            _matchBranchBody = body'
                          }
                    )
                    branches
                    (drop n (zipExact binders' args'))
                )
          }
  NPi (Pi i bi b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [bi ^. binderType, b],
        _nodeChildBindersNum = [0, 1],
        _nodeChildBindersInfo = [[], [bi]],
        _nodeReassemble = \i' _ binders' args' ->
          -- NOTE the binder type here is treated as a node
          -- TODO should we get the type from args' or binders'?
          let ty :: Type
              ty = hd args'
              bi' :: Binder
              bi' = set binderType ty (hd (binders' !! 1))
           in mkPi i' bi' (args' !! 1)
      }
  NUniv (Univ i l) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ _ -> mkUniv i' l
      }
  NTyp (TypeConstr i sym args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = args,
        _nodeChildBindersNum = map (const 0) args,
        _nodeChildBindersInfo = map (const []) args,
        _nodeReassemble = \i' _ _ args' -> mkTypeConstr i' sym args'
      }
  NPrim (TypePrim i prim) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ _ -> mkTypePrim i' prim
      }
  NDyn (Dynamic i) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeChildBindersNum = [],
        _nodeChildBindersInfo = [],
        _nodeReassemble = \i' _ _ _ -> mkDynamic i'
      }
  Closure env (Lambda i bi b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = b : env,
        _nodeChildBindersNum = 1 : map (const 0) env,
        _nodeChildBindersInfo = [bi] : map (const []) env,
        _nodeReassemble = \i' _ _ args' -> Closure (tl args') (Lambda i' bi (hd args'))
      }
  where
    setPatternsInfos :: [Binder] -> [Info] -> [Pattern] -> [Pattern]
    setPatternsInfos binders infos = snd . setPatternsInfos' binders infos
      where
        setPatternsInfos' :: [Binder] -> [Info] -> [Pattern] -> (([Binder], [Info]), [Pattern])
        setPatternsInfos' bs is [] = ((bs, is), [])
        setPatternsInfos' bs is (p : ps) =
          let ((bs', is'), p') = setPatInfos bs is p
              (bis'', ps') = setPatternsInfos' bs' is' ps
           in (bis'', p' : ps')

        setPatInfos :: [Binder] -> [Info] -> Pattern -> (([Binder], [Info]), Pattern)
        setPatInfos bs is = \case
          PatWildcard x ->
            ((bs, tl is), PatWildcard (x {_patternWildcardInfo = hd is}))
          PatBinder x ->
            ((tl bs, is), PatBinder (x {_patternBinder = hd bs}))
          PatConstr x ->
            let (bis', ps) = setPatternsInfos' bs (tl is) (x ^. patternConstrArgs)
             in (bis', PatConstr (x {_patternConstrInfo = hd is, _patternConstrArgs = ps}))

    hd :: [a] -> a
    hd = List.head

    tl :: [a] -> [a]
    tl = List.tail

reassemble :: Node -> [Node] -> Node
reassemble n = (d ^. nodeReassemble) (d ^. nodeInfo) (d ^. nodeSubinfos) (d ^. nodeChildBindersInfo)
  where
    d :: NodeDetails
    d = destruct n

children :: Node -> [Node]
children = (^. nodeChildren) . destruct

-- | children together with the number of binders
bchildren :: Node -> [(Int, Node)]
bchildren n =
  let ni = destruct n
   in zipExact (ni ^. nodeChildBindersNum) (ni ^. nodeChildren)

-- | shallow children: not under binders
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
        return ((ni ^. nodeReassemble) i' is' (ni ^. nodeChildBindersInfo) (ni ^. nodeChildren))

modifyInfo :: (Info -> Info) -> Node -> Node
modifyInfo f n = runIdentity $ modifyInfoM (pure . f) n
