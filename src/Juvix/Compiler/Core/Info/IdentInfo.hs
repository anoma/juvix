module Juvix.Compiler.Core.Info.IdentInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

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
      NIdt (Ident i sym) -> mkIdent (Info.insert fvi i) sym
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
