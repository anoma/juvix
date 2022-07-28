module Juvix.Core.Info.BinderInfo where

import Juvix.Core.Prelude
import Juvix.Core.Type

data BinderInfo = BinderInfo
  { _infoName :: Name,
    _infoType :: Type
  }

kBinderInfo :: Key BinderInfo
kBinderInfo = Proxy

newtype CaseBinderInfo = CaseBinderInfo
  { _infoBranchBinders :: [[BinderInfo]]
  }

kCaseBinderInfo :: Key CaseBinderInfo
kCaseBinderInfo = Proxy

makeLenses ''BinderInfo
makeLenses ''CaseBinderInfo
