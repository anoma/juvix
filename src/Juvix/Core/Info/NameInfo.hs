module Juvix.Core.Info.NameInfo where

import Juvix.Core.Prelude

newtype NameInfo = NameInfo { _infoName :: Name }

kNameInfo :: Key NameInfo
kNameInfo = Proxy

makeLenses ''NameInfo
