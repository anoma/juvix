module Juvix.Core.Language.Info.IdentInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Core.Extra
import Juvix.Core.Language
import Juvix.Core.Language.Info qualified as Info

newtype IdentInfo = IdentInfo
  { -- map symbols to the number of their occurrences
    _infoIdents :: HashMap Symbol Int
  }

instance IsInfo IdentInfo

kIdentInfo :: Key IdentInfo
kIdentInfo = Proxy

makeLenses ''IdentInfo

computeIdentInfo :: Node -> Node
computeIdentInfo = umap go
  where
    go :: Node -> Node
    go n = case n of
      Ident i sym -> Ident (Info.insert fvi i) sym
        where
          fvi = IdentInfo (HashMap.singleton sym 1)
      _ -> modifyInfo (Info.insert fvi) n
        where
          fvi =
            IdentInfo $
              foldr
                (HashMap.unionWith (+) . (^. infoIdents) . getIdentInfo)
                mempty
                (children n)

getIdentInfo :: Node -> IdentInfo
getIdentInfo = Info.lookupDefault (IdentInfo mempty) . getInfo

identOccurrences :: Symbol -> Node -> Int
identOccurrences sym = fromMaybe 0 . HashMap.lookup sym . (^. infoIdents) . getIdentInfo
