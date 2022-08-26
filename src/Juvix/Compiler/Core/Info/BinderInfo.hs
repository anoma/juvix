module Juvix.Compiler.Core.Info.BinderInfo where

import Juvix.Compiler.Core.Language

data BinderInfo = BinderInfo
  { _infoName :: Name,
    _infoType :: Type
  }

instance IsInfo BinderInfo

kBinderInfo :: Key BinderInfo
kBinderInfo = Proxy

newtype CaseBinderInfo = CaseBinderInfo
  { _infoBranchBinders :: [[BinderInfo]]
  }

instance IsInfo CaseBinderInfo

kCaseBinderInfo :: Key CaseBinderInfo
kCaseBinderInfo = Proxy

makeLenses ''BinderInfo
makeLenses ''CaseBinderInfo
