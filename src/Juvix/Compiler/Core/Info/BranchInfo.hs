module Juvix.Compiler.Core.Info.BranchInfo where

import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language.Base

newtype BranchInfo = BranchInfo
  { _infoTagName :: Name
  }

instance IsInfo BranchInfo

kBranchInfo :: Key BranchInfo
kBranchInfo = Proxy

makeLenses ''BranchInfo

getInfoTagName :: Info -> Maybe Name
getInfoTagName i = case Info.lookup kBranchInfo i of
  Just BranchInfo {..} -> Just _infoTagName
  Nothing -> Nothing

setInfoTagName :: Name -> Info -> Info
setInfoTagName = Info.insert . BranchInfo
