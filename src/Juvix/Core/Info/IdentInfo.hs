module Juvix.Core.Info.IdentInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Core.Info qualified as Info
import Juvix.Core.Node
import Juvix.Core.Prelude

newtype IdentInfo = IdentInfo
  { -- map symbols to the number of their occurrences
    _infoIdents :: HashMap Symbol Int
  }

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
              HashMap.unions
                ( map
                    ( \n' ->
                        Info.lookupDefault
                          (IdentInfo mempty)
                          (getInfo n')
                          ^. infoIdents
                    )
                    (children n)
                )
