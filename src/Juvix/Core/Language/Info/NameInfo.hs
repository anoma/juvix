module Juvix.Core.Language.Info.NameInfo where

import Juvix.Core.Language.Base

newtype NameInfo = NameInfo {_infoName :: Name}

instance IsInfo NameInfo

kNameInfo :: Key NameInfo
kNameInfo = Proxy

makeLenses ''NameInfo
