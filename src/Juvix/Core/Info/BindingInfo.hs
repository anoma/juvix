module Juvix.Core.Info.BindingInfo where

import Juvix.Core.Prelude
import Juvix.Core.Type

data BindingInfo = BindingInfo
  { _infoName :: Name,
    _infoType :: Type
  }

kBindingInfo :: Key BindingInfo
kBindingInfo = Proxy

makeLenses ''BindingInfo
