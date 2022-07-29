module Juvix.Core.Info.FreeVarsInfo where

-- NOTE: IntMap could be better here and in other places in terms of performance
-- (by a constant factor). This might matter (but not very much) in the future -
-- we will be running these transformations on core representations of entire
-- Juvix functions (or even bigger units of compilation).
import Data.HashMap.Strict qualified as HashMap
import Juvix.Core.Info qualified as Info
import Juvix.Core.Node
import Juvix.Core.Prelude

newtype FreeVarsInfo = FreeVarsInfo
  { -- map free variables to the number of their occurrences
    _infoFreeVars :: HashMap Index Int
  }

kFreeVarsInfo :: Key FreeVarsInfo
kFreeVarsInfo = Proxy

makeLenses ''FreeVarsInfo

computeFreeVarsInfo :: Node -> Node
computeFreeVarsInfo = umapN go
  where
    go :: Index -> Node -> Node
    go k n = case n of
      Var i idx | idx >= k -> Var (Info.insert fvi i) idx
        where
          fvi = FreeVarsInfo (HashMap.singleton (idx - k) 1)
      _ -> modifyInfo (Info.insert fvi) n
        where
          fvi =
            FreeVarsInfo $
              foldr
                ( \(m, n') acc ->
                    HashMap.unionWith (+) acc $
                      HashMap.mapKeys (\j -> j - m) $
                        HashMap.filterWithKey
                          (\j _ -> j < m)
                          (getFreeVarsInfo n' ^. infoFreeVars)
                )
                mempty
                (bchildren n)

getFreeVarsInfo :: Node -> FreeVarsInfo
getFreeVarsInfo = fromJust . Info.lookup kFreeVarsInfo . getInfo

freeVarOccurrences :: Index -> Node -> Int
freeVarOccurrences idx n = fromMaybe 0 (HashMap.lookup idx (getFreeVarsInfo n ^. infoFreeVars))
