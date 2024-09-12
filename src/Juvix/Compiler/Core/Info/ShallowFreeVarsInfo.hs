module Juvix.Compiler.Core.Info.ShallowFreeVarsInfo where

import Data.Map qualified as Map
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info

newtype ShallowFreeVarsInfo = ShallowFreeVarsInfo
  { -- map free variables to the number of their shallow occurrences (not under binders)
    _infoShallowFreeVars :: Map Index Int
  }

instance IsInfo ShallowFreeVarsInfo

kShallowFreeVarsInfo :: Key ShallowFreeVarsInfo
kShallowFreeVarsInfo = Proxy

makeLenses ''ShallowFreeVarsInfo

-- | Computes shallow free variable info for each subnode. Assumption: no
-- subnode is a closure.
computeShallowFreeVarsInfo :: Node -> Node
computeShallowFreeVarsInfo = umap go
  where
    go :: Node -> Node
    go node = case node of
      NVar Var {..} ->
        mkVar (Info.insert fvi _varInfo) _varIndex
        where
          fvi = ShallowFreeVarsInfo (Map.singleton _varIndex 1)
      _ ->
        modifyInfo (Info.insert fvi) node
        where
          fvi =
            ShallowFreeVarsInfo $
              foldr
                ( \NodeChild {..} acc ->
                    if
                        | _childBindersNum == 0 ->
                            Map.unionWith (+) acc (getShallowFreeVarsInfo _childNode ^. infoShallowFreeVars)
                        | otherwise ->
                            acc
                )
                mempty
                (children node)

getShallowFreeVarsInfo :: Node -> ShallowFreeVarsInfo
getShallowFreeVarsInfo = fromJust . Info.lookup kShallowFreeVarsInfo . getInfo

shallowFreeVarOccurrences :: Index -> Node -> Int
shallowFreeVarOccurrences idx n = fromMaybe 0 (Map.lookup idx (getShallowFreeVarsInfo n ^. infoShallowFreeVars))
