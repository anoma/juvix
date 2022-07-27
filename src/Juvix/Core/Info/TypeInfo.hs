module Juvix.Core.Info.TypeInfo where

import Juvix.Core.Prelude
import Juvix.Core.Type

newtype TypeInfo = TypeInfo { _infoType :: Type }

kTypeInfo :: Key TypeInfo
kTypeInfo = Proxy

makeLenses ''TypeInfo
