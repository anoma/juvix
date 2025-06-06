module Juvix.Compiler.Core.Extra.Base
  ( module Juvix.Compiler.Core.Extra.Base,
    module Juvix.Compiler.Core.Data.BinderList,
  )
where

import Data.Functor.Identity
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NameInfo (setInfoName)
import Juvix.Compiler.Core.Language

{------------------------------------------------------------------------}
{- Node constructors -}

mkVar :: Info -> Index -> Node
mkVar i idx = NVar (Var i idx)

mkVarN :: Text -> Index -> Node
mkVarN name idx = NVar (Var (setInfoName name mempty) idx)

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

-- TODO use this in Core/Translation/FromInternal.hs

-- | If b is a builtin with arguments ty1 ty2
-- it returns: `\lambda (x1 : ty1) (x2 : ty2) := b x1 x2`
mkBuiltinExpanded' :: BuiltinOp -> [Type] -> Node
mkBuiltinExpanded' b argTys =
  let numArgs = length argTys
      prefixTypes = length (takeWhile isUniverse argTys)
      app = mkBuiltinApp' b [mkVar' argIx | argIx <- reverse [0 .. numArgs - 1 - prefixTypes]]
   in if
          | builtinOpArgsNum b == numArgs - prefixTypes -> mkLambdas' argTys app
          | otherwise ->
              impossibleError
                ( "unexpected number of args to builtin "
                    <> show b
                )

mkLambda' :: Type -> Node -> Node
mkLambda' ty = mkLambda Info.empty (mkBinder' ty)

mkLambda'' :: Binder -> Node -> Node
mkLambda'' = mkLambda Info.empty

mkLambdas :: [Info] -> [Binder] -> Node -> Node
mkLambdas is bs n = foldl' (flip (uncurry mkLambda)) n (reverse (zipExact is bs))

mkLambdas' :: [Type] -> Node -> Node
mkLambdas' tys n = foldl' (flip mkLambda') n (reverse tys)

mkLambdas'' :: [Binder] -> Node -> Node
mkLambdas'' bs n = foldl' (flip mkLambda'') n (reverse bs)

mkLetItem :: Text -> Type -> Node -> LetItem
mkLetItem name ty = LetItem (mkBinder name ty)

mkLetItem' :: Type -> Node -> LetItem
mkLetItem' ty = LetItem (mkBinder' ty)

mkLet :: Info -> Binder -> Node -> Node -> Node
mkLet i bi v b = NLet (Let i (LetItem bi v) b)

mkLet' :: Type -> Node -> Node -> Node
mkLet' ty = mkLet Info.empty (mkBinder' ty)

mkLets :: [LetItem] -> Node -> Node
mkLets tvs n = foldl' (\n' itm -> NLet (Let mempty itm n')) n (reverse tvs)

mkLets' :: [(Type, Node)] -> Node -> Node
mkLets' tvs n = foldl' (\n' (ty, v) -> mkLet' ty v n') n (reverse tvs)

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

mkIfs :: Symbol -> [(Info, Node, Node)] -> Node -> Node
mkIfs sym = \case
  [] -> id
  ((i, v, b) : rest) -> mkIf i sym v b . mkIfs sym rest

mkIfs' :: Symbol -> [(Node, Node)] -> Node -> Node
mkIfs' sym = mkIfs sym . map (\(v, b) -> (Info.empty, v, b))

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

mkTypeField :: Info -> Type
mkTypeField i = mkTypePrim i PrimField

mkTypeField' :: Type
mkTypeField' = mkTypeField Info.empty

mkTypeUInt8 :: Info -> Type
mkTypeUInt8 i = mkTypePrim i primitiveUInt8

mkTypeUInt8' :: Type
mkTypeUInt8' = mkTypeUInt8 Info.empty

mkTypeByteArray :: Info -> Type
mkTypeByteArray i = mkTypePrim i PrimByteArray

mkTypeByteArray' :: Type
mkTypeByteArray' = mkTypeByteArray Info.empty

mkTypeRandomGenerator :: Info -> Type
mkTypeRandomGenerator i = mkTypePrim i PrimRandomGenerator

mkTypeRandomGenerator' :: Type
mkTypeRandomGenerator' = mkTypeRandomGenerator Info.empty

mkDynamic :: Info -> Type
mkDynamic i = NDyn (DynamicTy i)

mkDynamic' :: Type
mkDynamic' = mkDynamic Info.empty

mkBottom :: Info -> Type -> Node
mkBottom _bottomInfo _bottomType = NBot Bottom {..}

mkBottom' :: Node
mkBottom' = mkBottom mempty mkDynamic'

{------------------------------------------------------------------------}
{- functions on Type -}

-- | Unfold a type into the target and the arguments (left-to-right)
unfoldPi :: Type -> ([PiLhs], Type)
unfoldPi ty = case ty of
  NPi (Pi i bi r) ->
    let (args, target) = unfoldPi r
     in (PiLhs i bi : args, target)
  _ -> ([], ty)

unfoldPi' :: Type -> ([Type], Type)
unfoldPi' = first (map (^. piLhsBinder . binderType)) . unfoldPi

typeArgs :: Type -> [Type]
typeArgs = fst . unfoldPi'

typeTarget :: Type -> Type
typeTarget = snd . unfoldPi

typeArgsBinders :: Type -> [Binder]
typeArgsBinders = map (^. piLhsBinder) . fst . unfoldPi

isDynamic :: Type -> Bool
isDynamic = \case
  NDyn {} -> True
  _ -> False

isInductive :: Type -> Bool
isInductive = \case
  NTyp {} -> True
  _ -> False

isTypePrim :: Type -> Bool
isTypePrim = \case
  NPrim {} -> True
  _ -> False

isTypeInteger :: Type -> Bool
isTypeInteger = \case
  NPrim (TypePrim _ (PrimInteger _)) -> True
  _ -> False

isTypeField :: Type -> Bool
isTypeField = \case
  NPrim (TypePrim _ PrimField) -> True
  _ -> False

isTypeBool :: Type -> Bool
isTypeBool = \case
  NPrim (TypePrim _ (PrimBool _)) -> True
  _ -> False

isUniverse :: Type -> Bool
isUniverse = \case
  NUniv {} -> True
  _ -> False

-- | `expandType argtys ty` expands the dynamic target of `ty` to match the
-- number of arguments with types specified by `argstys`. For example,
-- `expandType [int, string] (int -> any) = int -> string -> any`.
expandType :: [Binder] -> Type -> Type
expandType argtys ty =
  let (tyargs, target) = unfoldPi ty
   in if
          | length tyargs >= length argtys ->
              ty
          | isDynamic target ->
              rePis tyargs (mkPis (drop (length tyargs) argtys) target)
          | otherwise ->
              impossible

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

lambdaBinders :: Node -> [Binder]
lambdaBinders = map (^. lambdaLhsBinder) . fst . unfoldLambdas

isConstructorApp :: Node -> Bool
isConstructorApp node = case node of
  NCtr {} -> True
  _ -> False

{------------------------------------------------------------------------}
{- functions on Pattern -}

getPatternBinder :: Pattern -> Binder
getPatternBinder = \case
  PatConstr PatternConstr {..} -> _patternConstrBinder
  PatWildcard PatternWildcard {..} -> _patternWildcardBinder

getPatternBinders :: Pattern -> [Binder]
getPatternBinders = reverse . go []
  where
    go :: [Binder] -> Pattern -> [Binder]
    go acc = \case
      PatConstr PatternConstr {..} -> foldl' go (_patternConstrBinder : acc) _patternConstrArgs
      PatWildcard PatternWildcard {..} -> _patternWildcardBinder : acc

getPatternExtraBinders :: Pattern -> [Binder]
getPatternExtraBinders = \case
  PatConstr PatternConstr {..} -> concatMap getPatternBinders _patternConstrArgs
  PatWildcard {} -> []

getPatternInfos :: Pattern -> [Info]
getPatternInfos = reverse . go []
  where
    go :: [Info] -> Pattern -> [Info]
    go acc = \case
      PatConstr PatternConstr {..} -> foldl' go (_patternConstrInfo : acc) _patternConstrArgs
      PatWildcard PatternWildcard {..} -> _patternWildcardInfo : acc

isPatWildcard :: Pattern -> Bool
isPatWildcard = \case
  PatWildcard {} -> True
  PatConstr {} -> False

isPatConstr :: Pattern -> Bool
isPatConstr = \case
  PatConstr {} -> True
  PatWildcard {} -> False

{------------------------------------------------------------------------}
{- match branch -}

isMatchBranchRhsExpression :: MatchBranch -> Bool
isMatchBranchRhsExpression MatchBranch {..} =
  case _matchBranchRhs of
    MatchBranchRhsExpression {} -> True
    MatchBranchRhsIfs {} -> False

isMatchBranchRhsIf :: MatchBranch -> Bool
isMatchBranchRhsIf = not . isMatchBranchRhsExpression

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
        mkBranch :: Info -> CaseBranch -> Sem '[Input Node] CaseBranch
        mkBranch nfo' br = do
          b' <- inputJust
          let nBinders = br ^. caseBranchBindersNum
          tys' <- replicateM nBinders inputJust
          return
            br
              { _caseBranchInfo = nfo',
                _caseBranchBinders = zipWithExact (set binderType) tys' (br ^. caseBranchBinders),
                _caseBranchBody = b'
              }
        mkBranches :: [Info] -> [Node] -> [CaseBranch]
        mkBranches is' allNodes' =
          run
            . runInputList allNodes'
            $ sequence
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
              [ brs
                  ++ reverse (foldl' (\acc b -> manyBinders (take (length acc) bis) (b ^. binderType) : acc) [] bis)
                | (bis, brs) <- branchChildren
              ]
          where
            branchChildren :: [([Binder], [NodeChild])]
            branchChildren =
              [ (binders, map (manyBinders binders) (branchRhsChildren (br ^. matchBranchRhs)))
                | br <- branches,
                  let binders = concatMap getPatternBinders (toList (br ^. matchBranchPatterns))
              ]

            branchRhsChildren :: MatchBranchRhs -> [Node]
            branchRhsChildren = \case
              MatchBranchRhsExpression e -> [e]
              MatchBranchRhsIfs ifs -> concatMap sideIfBranchChildren ifs

            sideIfBranchChildren :: SideIfBranch -> [Node]
            sideIfBranchChildren SideIfBranch {..} =
              [_sideIfBranchCondition, _sideIfBranchBody]

        branchInfos :: [Info]
        branchInfos =
          concat
            [ br
                ^. matchBranchInfo
                : getSideIfBranchInfos (br ^. matchBranchRhs)
                ++ concatMap getPatternInfos (br ^. matchBranchPatterns)
              | br <- branches
            ]

        getSideIfBranchInfos :: MatchBranchRhs -> [Info]
        getSideIfBranchInfos = \case
          MatchBranchRhsExpression _ -> []
          MatchBranchRhsIfs ifs -> map getSideIfBranchInfos' (toList ifs)
          where
            getSideIfBranchInfos' :: SideIfBranch -> Info
            getSideIfBranchInfos' SideIfBranch {..} = _sideIfBranchInfo

        -- sets the infos and the binder types in the patterns
        setPatternsInfos :: forall r. (Members '[Input Info, Input Node] r) => NonEmpty Pattern -> Sem r (NonEmpty Pattern)
        setPatternsInfos = mapM goPattern
          where
            goPattern :: Pattern -> Sem r Pattern
            goPattern = \case
              PatWildcard x -> do
                i' <- inputJust
                ty <- inputJust
                return (PatWildcard (over patternWildcardBinder (set binderType ty) (set patternWildcardInfo i' x)))
              PatConstr x -> do
                i' <- inputJust
                ty <- inputJust
                args' <- mapM goPattern (x ^. patternConstrArgs)
                return (PatConstr (over patternConstrBinder (set binderType ty) (set patternConstrInfo i' (set patternConstrArgs args' x))))
     in NodeDetails
          { _nodeInfo = i,
            _nodeSubinfos = branchInfos,
            _nodeChildren = allNodes,
            _nodeReassemble = someChildrenI $ \i' is' chs' ->
              let mkBranch :: MatchBranch -> Sem '[Input Node, Input Info] MatchBranch
                  mkBranch br = do
                    bi' <- inputJust
                    b' <- mkBranchRhs (br ^. matchBranchRhs)
                    pats' <- setPatternsInfos (br ^. matchBranchPatterns)
                    return
                      br
                        { _matchBranchInfo = bi',
                          _matchBranchPatterns = pats',
                          _matchBranchRhs = b'
                        }
                  mkBranchRhs :: MatchBranchRhs -> Sem '[Input Node, Input Info] MatchBranchRhs
                  mkBranchRhs = \case
                    MatchBranchRhsExpression _ -> do
                      e' <- inputJust
                      return (MatchBranchRhsExpression e')
                    MatchBranchRhsIfs ifs -> do
                      ifs' <- mkSideIfs ifs
                      return (MatchBranchRhsIfs ifs')
                  mkSideIfs :: NonEmpty SideIfBranch -> Sem '[Input Node, Input Info] (NonEmpty SideIfBranch)
                  mkSideIfs brs =
                    mapM mkSideIfBranch brs
                  mkSideIfBranch :: SideIfBranch -> Sem '[Input Node, Input Info] SideIfBranch
                  mkSideIfBranch _ = do
                    _sideIfBranchInfo <- inputJust
                    _sideIfBranchCondition <- inputJust
                    _sideIfBranchBody <- inputJust
                    return SideIfBranch {..}
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
  NDyn (DynamicTy i) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [],
        _nodeReassemble = noChildren $ \i' -> mkDynamic i'
      }
  NBot (Bottom i ty) ->
    NodeDetails
      { _nodeInfo = i,
        _nodeSubinfos = [],
        _nodeChildren = [noBinders ty],
        _nodeReassemble = oneChild mkBottom
      }
  Closure env n ->
    NodeDetails
      { _nodeInfo = mempty,
        _nodeSubinfos = [],
        _nodeChildren = manyBinders binders n : map noBinders env,
        _nodeReassemble = someChildren $ \_ (n' :| env') ->
          Closure env' n'
      }
    where
      binders = replicate (length env) (mkBinder' mkDynamic')

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
