module Juvix.Compiler.Core.Info.BinderInfo where

import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

newtype BinderInfo = BinderInfo {_infoBinder :: Info}

instance IsInfo BinderInfo

kBinderInfoData :: Key BinderInfo
kBinderInfoData = Proxy

newtype CaseBinderInfo = CaseBinderInfo
  { _infoBranchBinders :: [[Info]]
  }

instance IsInfo CaseBinderInfo

kCaseBinderInfo :: Key CaseBinderInfo
kCaseBinderInfo = Proxy

makeLenses ''BinderInfo
makeLenses ''CaseBinderInfo

getInfoBinder :: Info -> Info
getInfoBinder i =
  case Info.lookup kBinderInfoData i of
    Just (BinderInfo {..}) -> _infoBinder
    Nothing -> Info.empty
