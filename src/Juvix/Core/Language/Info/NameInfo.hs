module Juvix.Core.Language.Info.NameInfo where

import Juvix.Core.Prelude

newtype NameInfo = NameInfo {_infoName :: Name}

instance IsInfo NameInfo

kNameInfo :: Key NameInfo
kNameInfo = Proxy

makeLenses ''NameInfo
