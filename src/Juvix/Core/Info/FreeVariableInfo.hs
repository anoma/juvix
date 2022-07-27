module Juvix.Core.Info.FreeVariableInfo where

import Data.HashMap.Strict as HashMap
import Juvix.Core.Info qualified as Info
import Juvix.Core.Node
import Juvix.Core.Prelude

newtype FreeVariableInfo = FreeVariableInfo
  { -- map free variables to the number of their occurrences
    _infoFreeVars :: HashMap Index Int
  }

kFreeVariableInfo :: Key FreeVariableInfo
kFreeVariableInfo = Proxy

makeLenses ''FreeVariableInfo

computeFreeVariableInfo :: Node -> Node
computeFreeVariableInfo = umapN go
  where
    go :: Index -> Node -> Node
    go k n = case n of
      Var i idx | idx >= k -> Var (Info.insert fvi i) idx
        where
          fvi = FreeVariableInfo (HashMap.singleton (idx - k) 1)
      _ -> undefined
