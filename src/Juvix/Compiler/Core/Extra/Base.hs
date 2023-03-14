module Juvix.Compiler.Core.Extra.Base
  ( module Juvix.Compiler.Core.Extra.Base,
    module Juvix.Compiler.Core.Data.BinderList,
  )
where

import Data.Functor.Identity
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language
import Polysemy.Input

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

mkLambda :: Info -> Binder -> Node -> Node
mkLambda i bi b = NLam (Lambda i bi b)

mkLambda' :: Type -> Node -> Node
mkLambda' ty = mkLambda Info.empty (mkBinder' ty)

mkLambdas' :: [Type] -> Node -> Node
mkLambdas' tys n = foldl' (flip mkLambda') n (reverse tys)

mkLetItem' :: Type -> Node -> LetItem
mkLetItem' ty = LetItem (mkBinder' ty)

mkLet :: Info -> Binder -> Node -> Node -> Node
mkLet i bi v b = NLet (Let i (LetItem bi v) b)

mkLet' :: Type -> Node -> Node -> Node
mkLet' ty = mkLet Info.empty (mkBinder' ty)

mkLetRec :: Info -> NonEmpty LetItem -> Node -> Node
mkLetRec i vs b = NRec (LetRec i vs b)

mkLetRec' :: NonEmpty (Type, Node) -> Node -> Node
mkLetRec' = mkLetRec Info.empty . fmap (uncurry mkLetItem')

mkCase :: Info -> Symbol -> Node -> [CaseBranch] -> Maybe Node -> Node
mkCase i sym v bs def = NCase (Case i sym v bs def)

mkCase' :: Symbol -> Node -> [CaseBranch] -> Maybe Node -> Node
mkCase' = mkCase Info.empty

mkMatch :: Info -> NonEmpty Type -> Type -> NonEmpty Node -> [MatchBranch] -> Node
mkMatch i vtys rty vs bs = NMatch (Match i vtys rty vs bs)

mkMatch' :: NonEmpty Type -> Type -> NonEmpty Node -> [MatchBranch] -> Node
mkMatch' = mkMatch Info.empty

mkMatchBranch :: Info -> NonEmpty Pattern -> Node -> MatchBranch
mkMatchBranch = MatchBranch

mkMatchBranch' :: NonEmpty Pattern -> Node -> MatchBranch
mkMatchBranch' = MatchBranch mempty

mkIf :: Info -> Symbol -> Node -> Node -> Node -> Node
mkIf i sym v b1 b2 = mkCase i sym v [br] (Just b2)
  where
    br =
      CaseBranch
        { _caseBranchInfo = mempty,
          _caseBranchTag = BuiltinTag TagTrue,
          _caseBranchBinders = [],
          _caseBranchBindersNum = 0,
          _caseBranchBody = b1
        }

mkIf' :: Symbol -> Node -> Node -> Node -> Node
mkIf' = mkIf Info.empty

mkBinder :: Text -> Type -> Binder
mkBinder name ty = Binder name Nothing ty

mkBinder' :: Type -> Binder
mkBinder' ty = Binder "?" Nothing ty

{------------------------------------------------------------------------}
{- Type constructors -}

mkPi :: Info -> Binder -> Type -> Type
mkPi i bi b = NPi (Pi i bi b)

mkPi' :: Type -> Type -> Type
mkPi' = mkPi Info.empty . Binder "?" Nothing

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

mkSmallUniv :: Type
mkSmallUniv = mkUniv' (fromIntegral smallLevel)

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

isInductive :: Type -> Bool
isInductive NTyp {} = True
isInductive _ = False

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

reLambda :: LambdaLhs -> Node -> Node
reLambda lhs = mkLambda (lhs ^. lambdaLhsInfo) (lhs ^. lambdaLhsBinder)

reLambdas :: [LambdaLhs] -> Node -> Node
reLambdas is n = foldl' (flip reLambda) n (reverse is)

-- | useful with unfoldLambdasRev
reLambdasRev :: [LambdaLhs] -> Node -> Node
reLambdasRev is n = foldl' (flip reLambda) n is

mkLambdaB :: Binder -> Node -> Node
mkLambdaB = mkLambda mempty

mkLambdasB :: [Binder] -> Node -> Node
mkLambdasB is n = foldl' (flip mkLambdaB) n (reverse is)

-- | \x\y b gives ([y, x], b)
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

lambdaTypes :: Node -> [Type]
lambdaTypes = map (\LambdaLhs {..} -> _lambdaLhsBinder ^. binderType) . fst . unfoldLambdas

isType :: Node -> Bool
isType = \case
  NPi {} -> True
  NUniv {} -> True
  NPrim {} -> True
  NTyp {} -> True
  NDyn {} -> True
  NVar {} -> False
  NIdt {} -> False
  NCst {} -> False
  NApp {} -> False
  NBlt {} -> False
  NCtr {} -> False
  NLam {} -> False
  NLet {} -> False
  NRec {} -> False
  NCase {} -> False
  NMatch {} -> False
  Closure {} -> False

{------------------------------------------------------------------------}
{- functions on Pattern -}

getPatternParts :: Pattern -> [Either Binder Node]
getPatternParts = reverse . go []
  where
    go :: [Either Binder Node] -> Pattern -> [Either Binder Node]
    go acc = \case
      PatConstr PatternConstr {..} -> Right _patternConstrType : foldl' go acc _patternConstrArgs
      PatBinder p -> go (Left (p ^. patternBinder) : acc) (p ^. patternBinderPattern)
      PatWildcard {} -> acc

getPatternBinders :: Pattern -> [Binder]
getPatternBinders = lefts . getPatternParts

getPatternInfos :: Pattern -> [Info]
getPatternInfos = reverse . go []
  where
    go :: [Info] -> Pattern -> [Info]
    go acc = \case
      PatConstr PatternConstr {..} -> foldl' go (_patternConstrInfo : acc) _patternConstrArgs
      PatBinder PatternBinder {..} -> go acc _patternBinderPattern
      PatWildcard PatternWildcard {..} -> _patternWildcardInfo : acc

mapPatternBinders :: (Int -> Binder -> Binder) -> Pattern -> Pattern
mapPatternBinders f = snd . go 0
  where
    go :: Int -> Pattern -> (Int, Pattern)
    go k = \case
      PatConstr p ->
        let (k', rargs') =
              foldl'
                ( \(m, acc) pat ->
                    let (m', pat') = go m pat
                     in (m', pat' : acc)
                )
                (k, [])
                (p ^. patternConstrArgs)
         in (k', PatConstr p {_patternConstrArgs = reverse rargs'})
      PatBinder p ->
        let (k', pat') = go (k + 1) (p ^. patternBinderPattern)
         in ( k',
              PatBinder
                p
                  { _patternBinder = f k (p ^. patternBinder),
                    _patternBinderPattern = pat'
                  }
            )
      PatWildcard p -> (k, PatWildcard p)

{------------------------------------------------------------------------}
{- generic Node destruction -}

data NodeChild = NodeChild
  { -- | immediate child of some node
    _childNode :: Node,
    -- | Binders introduced by the child
    _childBinders :: [Binder],
    -- | length of `_childBinders`
    _childBindersNum :: Int
  }

makeLenses ''NodeChild

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
    _nodeChildren :: [NodeChild],
    -- | 'nodeReassemble' reassembles the node from the info, the subinfos and
    -- the children (which should be in the same fixed order as in the
    -- 'nodeSubinfos' and 'nodeChildren' components).
    _nodeReassemble :: Info -> [Info] -> [Node] -> Node
  }

makeLenses ''NodeDetails

{-# INLINE noBinders #-}
noBinders :: Node -> NodeChild
noBinders n =
  NodeChild
    { _childNode = n,
      _childBinders = [],
      _childBindersNum = 0
    }

{-# INLINE oneBinder #-}
oneBinder :: Binder -> Node -> NodeChild
oneBinder bi n =
  NodeChild
    { _childNode = n,
      _childBinders = [bi],
      _childBindersNum = 1
    }

{-# INLINE manyBinders #-}
manyBinders :: [Binder] -> Node -> NodeChild
manyBinders bis n =
  NodeChild
    { _childNode = n,
      _childBinders = bis,
      _childBindersNum = length bis
    }

type Reassemble = Info -> [Info] -> [Node] -> Node

{-# INLINE noChildren #-}
noChildren :: (Info -> Node) -> Reassemble
noChildren f i _ _ = f i

{-# INLINE oneChild #-}
oneChild :: (Info -> Node -> Node) -> Reassemble
oneChild f i _ ch = case ch of
  [c] -> f i c
  _ -> impossible

{-# INLINE twoChildren #-}
twoChildren :: (Info -> Node -> Node -> Node) -> Reassemble
twoChildren f i _ ch = case ch of
  [l, r] -> f i l r
  _ -> impossible

{-# INLINE threeChildren #-}
threeChildren :: (Info -> Node -> Node -> Node -> Node) -> Reassemble
threeChildren f i _ ch = case ch of
  [a, b, c] -> f i a b c
  _ -> impossible

{-# INLINE manyChildren #-}
manyChildren :: (Info -> [Node] -> Node) -> Reassemble
manyChildren f i _ = f i

{-# INLINE someChildren #-}
someChildren :: (Info -> NonEmpty Node -> Node) -> Reassemble
someChildren f i _ = f i . nonEmpty'

{-# INLINE someChildrenI #-}
someChildrenI :: (Info -> [Info] -> NonEmpty Node -> Node) -> Reassemble
someChildrenI f i is = f i is . nonEmpty'

{-# INLINE twoManyChildrenI #-}
twoManyChildrenI :: (Info -> [Info] -> Node -> Node -> [Node] -> Node) -> Reassemble
twoManyChildrenI f i is = \case
  (x : y : xs) -> f i is x y xs
  _ -> impossible

{-# INLINE input' #-}
input' :: (Members '[Input (Maybe a)] r) => Sem r a
input' = fmap fromJust input

-- | Destruct a node into NodeDetails. This is an ugly internal function used to
-- implement more high-level accessors and recursors.
destruct :: Node -> NodeDetails
destruct = \case
  NVar (Var i idx) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeReassemble = noChildren $ \i' -> mkVar i' idx
      }
  NIdt (Ident i sym) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeReassemble = noChildren $ \i' -> mkIdent i' sym
      }
  NCst (Constant i c) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeReassemble = noChildren $ \i' -> mkConstant i' c
      }
  NApp (App i l r) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = map noBinders [l, r],
        _nodeReassemble = twoChildren $ \i' l' r' -> mkApp i' l' r'
      }
  NBlt (BuiltinApp i op args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = map noBinders args,
        _nodeReassemble = manyChildren $ \i' args' -> mkBuiltinApp i' op args'
      }
  NCtr (Constr i tag args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = map noBinders args,
        _nodeReassemble = manyChildren $ \i' -> mkConstr i' tag
      }
  NLam (Lambda i bi b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [noBinders (bi ^. binderType), oneBinder bi b],
        _nodeReassemble = twoChildren $ \i' ty' b' ->
          let binder' :: Binder
              binder' = set binderType ty' bi
           in mkLambda i' binder' b'
      }
  NLet (Let i (LetItem bi v) b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [noBinders (bi ^. binderType), noBinders v, oneBinder bi b],
        _nodeReassemble = threeChildren $ \i' ty' v' b' ->
          let binder' :: Binder
              binder' = set binderType ty' bi
           in mkLet i' binder' v' b'
      }
  NRec (LetRec i vs b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren =
          let binders :: [Binder]
              values :: [Node]
              (binders, values) = unzip [(it ^. letItemBinder, it ^. letItemValue) | it <- toList vs]
              binderTypes :: [Type]
              binderTypes = map (^. binderType) binders
           in map (manyBinders binders) (b : values) ++ map noBinders binderTypes,
        _nodeReassemble = someChildren $ \i' (b' :| valuesTys') ->
          let numItems :: Int
              numItems = length vs
              tys' :: [Type]
              values' :: [Node]
              (values', tys') = splitAtExact numItems valuesTys'
              items' =
                nonEmpty'
                  [ LetItem (Binder name loc ty') v'
                    | (v', ty', name, loc) <-
                        zip4Exact
                          values'
                          tys'
                          (map (^. letItemBinder . binderName) (toList vs))
                          (map (^. letItemBinder . binderLocation) (toList vs))
                  ]
           in mkLetRec i' items' b'
      }
  NCase (Case i sym v brs mdef) ->
    let branchChildren :: [([Binder], NodeChild)]
        branchChildren =
          [ (binders, manyBinders binders (br ^. caseBranchBody))
            | br <- brs,
              let binders = br ^. caseBranchBinders
          ]
        -- in this list we have the bodies and the binder types interleaved
        allNodes :: [NodeChild]
        allNodes =
          concat
            [ br : reverse (foldl' (\r b -> manyBinders (take (length r) bi) (b ^. binderType) : r) [] bi)
              | (bi, br) <- branchChildren
            ]
        mkBranch :: Info -> CaseBranch -> Sem '[Input (Maybe Node)] CaseBranch
        mkBranch nfo' br = do
          b' <- input'
          let nBinders = br ^. caseBranchBindersNum
          tys' <- replicateM nBinders input'
          return
            br
              { _caseBranchInfo = nfo',
                _caseBranchBinders = zipWithExact (set binderType) tys' (br ^. caseBranchBinders),
                _caseBranchBody = b'
              }
        mkBranches :: [Info] -> [Node] -> [CaseBranch]
        mkBranches is' allNodes' =
          run $
            runInputList allNodes' $
              sequence
                [ mkBranch ci' br
                  | (ci', br) <- zipExact is' brs
                ]
     in case mdef of
          Nothing ->
            NodeDetails
              { _nodeInfo = i,
                _nodeSubinfos = map (^. caseBranchInfo) brs,
                _nodeChildren = noBinders v : allNodes,
                _nodeReassemble = someChildrenI $ \i' is' (v' :| allNodes') ->
                  mkCase i' sym v' (mkBranches is' allNodes') Nothing
              }
          Just def ->
            NodeDetails
              { _nodeInfo = i,
                _nodeSubinfos = map (^. caseBranchInfo) brs,
                _nodeChildren = noBinders v : noBinders def : allNodes,
                _nodeReassemble = twoManyChildrenI $ \i' is' v' def' allNodes' ->
                  mkCase i' sym v' (mkBranches is' allNodes') (Just def')
              }
  NMatch (Match i vtys rty vs branches) ->
    let allNodes :: [NodeChild]
        allNodes =
          noBinders rty
            : map noBinders (toList vtys)
            ++ map noBinders (toList vs)
            ++ concat
              [ br
                  : reverse (foldl' (\acc b -> manyBinders (take (length acc) (lefts bis)) (getNode b) : acc) [] bis)
                | (bis, br) <- branchChildren
              ]
          where
            branchChildren :: [([Either Binder Node], NodeChild)]
            branchChildren =
              [ (parts, manyBinders (lefts parts) (br ^. matchBranchBody))
                | br <- branches,
                  let parts = concatMap getPatternParts (toList (br ^. matchBranchPatterns))
              ]

            getNode :: Either Binder Node -> Node
            getNode = \case
              Left b -> b ^. binderType
              Right n -> n

        branchInfos :: [Info]
        branchInfos =
          concat
            [ br
                ^. matchBranchInfo
                : concatMap getPatternInfos (br ^. matchBranchPatterns)
              | br <- branches
            ]
        -- sets the infos and the binder types in the patterns
        setPatternsInfos :: forall r. (Members '[Input (Maybe Info), Input (Maybe Node)] r) => NonEmpty Pattern -> Sem r (NonEmpty Pattern)
        setPatternsInfos = mapM goPattern
          where
            goPattern :: Pattern -> Sem r Pattern
            goPattern = \case
              PatWildcard x -> do
                i' <- input'
                return (PatWildcard (set patternWildcardInfo i' x))
              PatBinder x -> do
                ty <- input'
                let _patternBinder = set binderType ty (x ^. patternBinder)
                _patternBinderPattern <- goPattern (x ^. patternBinderPattern)
                return (PatBinder PatternBinder {..})
              PatConstr x -> do
                i' <- input'
                args' <- mapM goPattern (x ^. patternConstrArgs)
                ty <- input'
                return (PatConstr (set patternConstrType ty (set patternConstrInfo i' (set patternConstrArgs args' x))))
     in NodeDetails
          { _nodeInfo = i,
            _nodeSubinfos = branchInfos,
            _nodeChildren = allNodes,
            _nodeReassemble = someChildrenI $ \i' is' chs' ->
              let mkBranch :: MatchBranch -> Sem '[Input (Maybe Node), Input (Maybe Info)] MatchBranch
                  mkBranch br = do
                    bi' <- input'
                    b' <- input'
                    pats' <- setPatternsInfos (br ^. matchBranchPatterns)
                    return
                      br
                        { _matchBranchInfo = bi',
                          _matchBranchPatterns = pats',
                          _matchBranchBody = b'
                        }
                  numVals = length vs
                  values' :: NonEmpty Node
                  valueTypes' :: NonEmpty Node
                  returnType' :: Node
                  branchesChildren' :: [Node]
                  returnType' = head chs'
                  (valueTypes', chs'') = first nonEmpty' (splitAtExact numVals (NonEmpty.tail chs'))
                  (values', branchesChildren') = first nonEmpty' (splitAtExact numVals chs'')
                  branches' :: [MatchBranch]
                  branches' =
                    run $
                      runInputList is' $
                        runInputList branchesChildren' $
                          mapM mkBranch branches
               in mkMatch
                    i'
                    valueTypes'
                    returnType'
                    values'
                    branches'
          }
  NPi (Pi i bi b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [noBinders (bi ^. binderType), oneBinder bi b],
        _nodeReassemble = twoChildren $ \i' bi' b' ->
          let binder' :: Binder
              binder' = set binderType bi' bi
           in mkPi i' binder' b'
      }
  NUniv (Univ i l) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeReassemble = noChildren $ \i' -> mkUniv i' l
      }
  NTyp (TypeConstr i sym args) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = map noBinders args,
        _nodeReassemble = manyChildren $ \i' -> mkTypeConstr i' sym
      }
  NPrim (TypePrim i prim) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeReassemble = noChildren $ \i' -> mkTypePrim i' prim
      }
  NDyn (Dynamic i) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeReassemble = noChildren $ \i' -> mkDynamic i'
      }
  Closure env (Lambda i bi b) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = oneBinder bi b : map noBinders env,
        _nodeReassemble = someChildren $ \i' (b' :| env') ->
          Closure env' (Lambda i' bi b')
      }

reassembleDetails :: NodeDetails -> [Node] -> Node
reassembleDetails d ns = (d ^. nodeReassemble) (d ^. nodeInfo) (d ^. nodeSubinfos) ns

reassemble :: Node -> [Node] -> Node
reassemble = reassembleDetails . destruct

children :: Node -> [NodeChild]
children = (^. nodeChildren) . destruct

childrenNodes :: Node -> [Node]
childrenNodes = map (^. childNode) . children

-- | shallow children: not under binders
schildren :: Node -> [Node]
schildren = map (^. childNode) . filter (\p -> null (p ^. childBinders)) . children

getInfo :: Node -> Info
getInfo = (^. nodeInfo) . destruct

modifyInfoM :: (Monad m) => (Info -> m Info) -> Node -> m Node
modifyInfoM f n =
  let ni = destruct n
   in do
        i' <- f (ni ^. nodeInfo)
        is' <- mapM f (ni ^. nodeSubinfos)
        return ((ni ^. nodeReassemble) i' is' (map (^. childNode) (ni ^. nodeChildren)))

modifyInfo :: (Info -> Info) -> Node -> Node
modifyInfo f n = runIdentity $ modifyInfoM (pure . f) n
