module Juvix.Compiler.Core.Info.NameInfo where

import Juvix.Compiler.Core.Language.Base

newtype NameInfo = NameInfo {_infoName :: Name}

instance IsInfo NameInfo

kNameInfo :: Key NameInfo
kNameInfo = Proxy

makeLenses ''NameInfo
