module Juvix.Compiler.Core.Extra.Utils
  ( module Juvix.Compiler.Core.Extra.Utils,
    module Juvix.Compiler.Core.Extra.Base,
    module Juvix.Compiler.Core.Extra.Recursors,
    module Juvix.Compiler.Core.Extra.Info,
    module Juvix.Compiler.Core.Extra.Equality,
    module Juvix.Compiler.Core.Extra.Recursors.Fold.Named,
    module Juvix.Compiler.Core.Extra.Recursors.Map.Named,
    module Juvix.Compiler.Core.Extra.Utils.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Set qualified as Set
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Equality
import Juvix.Compiler.Core.Extra.Info
import Juvix.Compiler.Core.Extra.Recursors
import Juvix.Compiler.Core.Extra.Recursors.Fold.Named
import Juvix.Compiler.Core.Extra.Recursors.Map.Named
import Juvix.Compiler.Core.Extra.Utils.Base
import Juvix.Compiler.Core.Language

isClosed :: Node -> Bool
isClosed = not . has freeVars

freeVarsSorted :: Node -> Set Var
freeVarsSorted n = Set.fromList (n ^.. freeVars)

freeVarsSet :: Node -> HashSet Var
freeVarsSet n = HashSet.fromList (n ^.. freeVars)

getIdents :: Node -> HashSet Ident
getIdents n = HashSet.fromList (n ^.. nodeIdents)

nodeIdents :: Traversal' Node Ident
nodeIdents f = ufoldA reassemble go
  where
    go = \case
      NIdt i -> NIdt <$> f i
      n -> pure n

-- | increase all free variable indices by a given value
shift :: Index -> Node -> Node
shift 0 = id
shift m = umapN go
  where
    go k = \case
      NVar v
        | v ^. varIndex >= k -> NVar (shiftVar m v)
      n -> n

-- | Prism for NRec
_NRec :: SimpleFold Node LetRec
_NRec f = \case
  NRec l -> NRec <$> f l
  n -> pure n

-- | Prism for NLam
_NLam :: SimpleFold Node Lambda
_NLam f = \case
  NLam l -> NLam <$> f l
  n -> pure n

-- | Fold over all of the transitive descendants of a Node, including itself.
cosmos :: SimpleFold Node Node
cosmos f = ufoldA reassemble f

-- | The list should not contain repeated indices.
-- if fv = x1, x2, .., xn
-- the result is of the form λx1 λx2 .. λ xn b
captureFreeVars :: [(Index, Binder)] -> Node -> Node
captureFreeVars freevars = goBinders freevars . mapFreeVars
  where
    mapFreeVars :: Node -> Node
    mapFreeVars = dmapN go
      where
        s :: HashMap Index Index
        s = HashMap.fromList (zip (reverse (map fst freevars)) [0 ..])
        go :: Index -> Node -> Node
        go k = \case
          NVar (Var i u)
            | Just v <- s ^. at (u - k) -> NVar (Var i (v + k))
          m -> m

    goBinders :: [(Index, Binder)] -> Node -> Node
    goBinders fv = case unsnoc fv of
      Nothing -> id
      Just (fvs, (idx, bin)) -> goBinders fvs . mkLambdaB (mapBinder idx bin)
      where
        indices = map fst fv
        mapBinder :: Index -> Binder -> Binder
        mapBinder binderIndex = over binderType (dmapN go)
          where
            go :: Index -> Node -> Node
            go k = \case
              NVar u
                | u ^. varIndex >= k ->
                    let uCtx = u ^. varIndex - k + binderIndex + 1
                        err = error ("impossible: could not find " <> show uCtx <> " in " <> show indices)
                        u' = length indices - 2 - fromMaybe err (elemIndex uCtx indices) + k
                     in NVar (set varIndex u' u)
              m -> m

-- | Captures all free variables of a node. It also returns the list of captured
-- variables in left-to-right order: if snd is of the form λxλy... then fst is
-- [x, y]
captureFreeVarsCtx :: BinderList Binder -> Node -> ([(Var, Binder)], Node)
captureFreeVarsCtx bl n =
  let assocs = freeVarsCtx bl n
   in (assocs, captureFreeVars (map (first (^. varIndex)) assocs) n)

freeVarsCtx' :: BinderList Binder -> Node -> [Var]
freeVarsCtx' bl = map fst . freeVarsCtx bl

-- | The output list does not contain repeated elements and is sorted by *decreasing* variable index.
-- The indices are relative to the given binder list
freeVarsCtx :: BinderList Binder -> Node -> [(Var, Binder)]
freeVarsCtx ctx =
  BL.lookupsSortedRev ctx . run . fmap fst . runOutputList . go . freeVarsSorted
  where
    go ::
      -- set of free variables relative to the original ctx
      Set Var ->
      Sem '[Output Var] ()
    go fv = case Set.minView fv of
      Nothing -> return ()
      Just (v, vs) -> do
        output v
        let idx = v ^. varIndex
            bi :: Binder = BL.lookup idx ctx
            fbi = freeVarsSorted (bi ^. binderType)
            freevarsbi' :: Set Var
            freevarsbi' = Set.mapMonotonic (over varIndex (+ (idx + 1))) fbi
        go (freevarsbi' <> vs)

-- | subst for multiple bindings
substs :: [Node] -> Node -> Node
substs t = umapN go
  where
    len = length t
    go k n = case n of
      NVar (Var i idx)
        | idx >= k, idx - k < len -> shift k (t !! (idx - k))
        | idx > k -> mkVar i (idx - len)
      _ -> n

-- | substitute a term t for the free variable with de Bruijn index 0, avoiding
-- variable capture; shifts all free variabes with de Bruijn index > 0 by -1 (as
-- if the topmost binder was removed)
subst :: Node -> Node -> Node
subst t = substs [t]

-- | `substDrop args argtys` drops `length args` from `argtys` and substitutes
-- the corresponding variables with `args`. For example:
-- ```
-- substDrop [Nat, Var 3] [Type, Type, Var 1, Var 1] =
--   [Nat, Var 4]
-- ``
substDrop :: [Node] -> [Node] -> [Node]
substDrop args argtys =
  reverse $ snd $ foldl' (\(args', acc) ty -> (mkVar' 0 : map (shift 1) args', substs args' ty : acc)) (reverse args, []) (drop k argtys)
  where
    k = length args

etaExpand :: [Type] -> Node -> Node
etaExpand [] n = n
etaExpand argtys n =
  mkLambdas' argtys (mkApps' (shift k n) (map mkVar' (reverse [0 .. k - 1])))
  where
    k = length argtys

convertClosures :: Node -> Node
convertClosures = umap go
  where
    go :: Node -> Node
    go n = case n of
      Closure env (Lambda i bi b) -> substEnv env (mkLambda i bi b)
      _ -> n

convertRuntimeNodes :: Node -> Node
convertRuntimeNodes = convertClosures

squashApps :: Node -> Node
squashApps = dmap go
  where
    go :: Node -> Node
    go n =
      let (l, args) = unfoldApps' n
       in case l of
            NCtr (Constr i tag args') -> mkConstr i tag (args' ++ args)
            NBlt (BuiltinApp i op args') -> mkBuiltinApp i op (args' ++ args)
            NTyp (TypeConstr i sym args') -> mkTypeConstr i sym (args' ++ args)
            _ -> n

binderFromArgumentInfo :: ArgumentInfo -> Binder
binderFromArgumentInfo a =
  Binder
    { _binderName = a ^. argumentName,
      _binderLocation = a ^. argumentLocation,
      _binderType = a ^. argumentType
    }

argumentInfoFromBinder :: Binder -> ArgumentInfo
argumentInfoFromBinder i =
  ArgumentInfo
    { _argumentName = i ^. binderName,
      _argumentLocation = i ^. binderLocation,
      _argumentType = i ^. binderType,
      _argumentIsImplicit = Explicit
    }

patternBinders :: SimpleFold Pattern PatternBinder
patternBinders f p = case p of
  PatWildcard {} -> pure p
  PatConstr c -> traverseOf patternConstrArgs (traverse (patternBinders f)) c $> p
  PatBinder b -> f b $> p

patternBindersNum :: Pattern -> Int
patternBindersNum = length . (^.. patternBinders)

patternType :: Pattern -> Node
patternType = \case
  PatWildcard w -> w ^. patternWildcardType
  PatBinder b -> b ^. patternBinder . binderType
  PatConstr c -> c ^. patternConstrType

builtinOpArgTypes :: BuiltinOp -> [Type]
builtinOpArgTypes = \case
  OpIntAdd -> [mkTypeInteger', mkTypeInteger']
  OpIntSub -> [mkTypeInteger', mkTypeInteger']
  OpIntMul -> [mkTypeInteger', mkTypeInteger']
  OpIntDiv -> [mkTypeInteger', mkTypeInteger']
  OpIntMod -> [mkTypeInteger', mkTypeInteger']
  OpIntLt -> [mkTypeInteger', mkTypeInteger']
  OpIntLe -> [mkTypeInteger', mkTypeInteger']
  OpEq -> [mkDynamic', mkDynamic']
  OpShow -> [mkDynamic']
  OpStrConcat -> [mkTypeString', mkTypeString']
  OpStrToInt -> [mkTypeString']
  OpTrace -> [mkDynamic', mkDynamic']
  OpFail -> [mkTypeString']
