module Juvix.Compiler.Core.Info.FreeVarsInfo where

import Data.Map qualified as Map
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

newtype FreeVarsInfo = FreeVarsInfo
  { -- map free variables to the number of their occurrences
    _infoFreeVars :: Map Index Int
  }

instance IsInfo FreeVarsInfo

kFreeVarsInfo :: Key FreeVarsInfo
kFreeVarsInfo = Proxy

makeLenses ''FreeVarsInfo

computeFreeVarsInfo :: Node -> Node
computeFreeVarsInfo = umap go
  where
    go :: Node -> Node
    go node = case node of
      NVar Var {..} ->
        mkVar (Info.insert fvi _varInfo) _varIndex
        where
          fvi = FreeVarsInfo (Map.singleton _varIndex 1)
      _ ->
        modifyInfo (Info.insert fvi) node
        where
          fvi =
            FreeVarsInfo $
              foldr
                ( \NodeChild {..} acc ->
                    Map.unionWith (+) acc $
                      Map.mapKeysMonotonic (\idx -> idx - _childBindersNum) $
                        Map.filterWithKey
                          (\idx _ -> idx >= _childBindersNum)
                          (getFreeVarsInfo _childNode ^. infoFreeVars)
                )
                mempty
                (children node)

getFreeVarsInfo :: Node -> FreeVarsInfo
getFreeVarsInfo = fromJust . Info.lookup kFreeVarsInfo . getInfo

freeVarOccurrences :: Index -> Node -> Int
freeVarOccurrences idx n = fromMaybe 0 (Map.lookup idx (getFreeVarsInfo n ^. infoFreeVars))
