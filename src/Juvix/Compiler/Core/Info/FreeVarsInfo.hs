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
computeFreeVarsInfo = umapN go
  where
    go :: Index -> Node -> Node
    go k n = case n of
      NVar (Var i idx) | idx >= k -> mkVar (Info.insert fvi i) idx
        where
          fvi = FreeVarsInfo (HashMap.singleton (idx - k) 1)
      _ -> modifyInfo (Info.insert fvi) n
        where
          fvi =
            FreeVarsInfo $
              foldr
                ( \n' acc ->
                    let m = n' ^. childBindersNum
                     in HashMap.unionWith (+) acc $
                          HashMap.mapKeys (\j -> j - m) $
                            HashMap.filterWithKey
                              (\j _ -> j < m)
                              (getFreeVarsInfo (n' ^. childNode) ^. infoFreeVars)
                )
                mempty
                (children n)

getFreeVarsInfo :: Node -> FreeVarsInfo
getFreeVarsInfo = fromJust . Info.lookup kFreeVarsInfo . getInfo

freeVarOccurrences :: Index -> Node -> Int
freeVarOccurrences idx n = fromMaybe 0 (HashMap.lookup idx (getFreeVarsInfo n ^. infoFreeVars))
