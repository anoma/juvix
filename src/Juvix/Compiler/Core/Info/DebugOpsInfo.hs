module Juvix.Compiler.Core.Info.DebugOpsInfo where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info

newtype DebugOpsInfo = DebugOpsInfo
  { _infoHasDebugOps :: Bool
  }

instance IsInfo DebugOpsInfo

kDebugOpsInfo :: Key DebugOpsInfo
kDebugOpsInfo = Proxy

makeLenses ''DebugOpsInfo

-- | Computes debug operations info for each subnode.
computeDebugOpsInfo :: Node -> Node
computeDebugOpsInfo = umap go
  where
    go :: Node -> Node
    go node
      | isDebugOp node =
          modifyInfo (Info.insert (DebugOpsInfo True)) node
      | otherwise =
          modifyInfo (Info.insert dbi) node
      where
        dbi =
          DebugOpsInfo
            . or
            . map (hasDebugOps . (^. childNode))
            $ children node

getDebugOpsInfo :: Node -> DebugOpsInfo
getDebugOpsInfo = fromJust . Info.lookup kDebugOpsInfo . getInfo

hasDebugOps :: Node -> Bool
hasDebugOps = (^. infoHasDebugOps) . getDebugOpsInfo
