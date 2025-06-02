module Juvix.Compiler.Core.Info.FreeVarsInfo where

import Data.Map qualified as Map
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info

newtype FreeVarsInfo = FreeVarsInfo
  { -- map free variables to the number of their occurrences
    _infoFreeVars :: Map Index Int
  }

instance IsInfo FreeVarsInfo

kFreeVarsInfo :: Key FreeVarsInfo
kFreeVarsInfo = Proxy

makeLenses ''FreeVarsInfo

-- | Computes free variable info for each subnode. Assumption: no subnode is a
-- closure.
computeFreeVarsInfo :: Node -> Node
computeFreeVarsInfo = computeFreeVarsInfo' 1

-- | `lambdaMultiplier` specifies how much to multiply the free variable count
-- for variables under lambdas
computeFreeVarsInfo' :: Int -> Node -> Node
computeFreeVarsInfo' lambdaMultiplier = umap go
  where
    go :: Node -> Node
    go node = case node of
      NVar Var {..} ->
        mkVar (Info.insert fvi _varInfo) _varIndex
        where
          fvi = FreeVarsInfo (Map.singleton _varIndex 1)
      NLam Lambda {..} ->
        modifyInfo (Info.insert fvi) node
        where
          fvi =
            FreeVarsInfo
              . fmap (* lambdaMultiplier)
              $ getFreeVars' 1 _lambdaBody
      _ ->
        modifyInfo (Info.insert fvi) node
        where
          fvi =
            FreeVarsInfo $
              foldr
                ( \NodeChild {..} acc ->
                    Map.unionWith (+) acc $
                      getFreeVars' _childBindersNum _childNode
                )
                mempty
                (children node)

    getFreeVars' :: Int -> Node -> Map Index Int
    getFreeVars' bindersNum node =
      Map.mapKeysMonotonic (\idx -> idx - bindersNum)
        . Map.filterWithKey (\idx _ -> idx >= bindersNum)
        $ getFreeVarsInfo node ^. infoFreeVars

getFreeVarsInfo :: Node -> FreeVarsInfo
getFreeVarsInfo = fromJust . Info.lookup kFreeVarsInfo . getInfo

getFreeVars :: Node -> [Index]
getFreeVars = Map.keys . Map.filter (> 0) . (^. infoFreeVars) . getFreeVarsInfo

freeVarOccurrences :: Index -> Node -> Int
freeVarOccurrences idx n = fromMaybe 0 (Map.lookup idx (getFreeVarsInfo n ^. infoFreeVars))

isClosed :: Node -> Bool
isClosed node = sum (Map.elems (getFreeVarsInfo node ^. infoFreeVars)) == 0
