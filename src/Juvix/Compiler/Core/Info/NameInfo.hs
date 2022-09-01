module Juvix.Compiler.Core.Info.NameInfo where

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Info qualified as Info

newtype NameInfo = NameInfo {_infoName :: Name}

instance IsInfo NameInfo

kNameInfo :: Key NameInfo
kNameInfo = Proxy

makeLenses ''NameInfo

getInfoName :: Info -> Maybe Name
getInfoName i =
  case Info.lookup kNameInfo i of
    Just (NameInfo {..}) -> Just _infoName
    Nothing -> Nothing
