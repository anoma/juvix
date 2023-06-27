module Juvix.Compiler.Core.Info.FreeVarsInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

newtype FreeVarsInfo = FreeVarsInfo
  { -- map free variables to the number of their occurrences
    _infoFreeVars :: HashMap Index Int
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
          fvi = FreeVarsInfo (HashMap.singleton _varIndex 1)
      _ ->
        modifyInfo (Info.insert fvi) node
        where
          fvi =
            FreeVarsInfo $
              foldr
                ( \NodeChild {..} acc ->
                    HashMap.unionWith (+) acc $
                      HashMap.mapKeys (\idx -> idx - _childBindersNum) $
                        HashMap.filterWithKey
                          (\idx _ -> idx >= _childBindersNum)
                          (getFreeVarsInfo _childNode ^. infoFreeVars)
                )
                mempty
                (children node)

getFreeVarsInfo :: Node -> FreeVarsInfo
getFreeVarsInfo = fromJust . Info.lookup kFreeVarsInfo . getInfo

freeVarOccurrences :: Index -> Node -> Int
freeVarOccurrences idx n = fromMaybe 0 (HashMap.lookup idx (getFreeVarsInfo n ^. infoFreeVars))
