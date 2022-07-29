module Juvix.Core.Language.Info.TypeInfo where

import Juvix.Core.Prelude
import Juvix.Core.Language.Type

newtype TypeInfo = TypeInfo {_infoType :: Type}

instance IsInfo TypeInfo

kTypeInfo :: Key TypeInfo
kTypeInfo = Proxy

makeLenses ''TypeInfo
