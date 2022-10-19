module Juvix.Compiler.Core.Info.NameInfo where

import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language.Base

newtype NameInfo = NameInfo {_infoName :: Name}

instance IsInfo NameInfo

kNameInfo :: Key NameInfo
kNameInfo = Proxy

makeLenses ''NameInfo

getInfoName :: Info -> Maybe Name
getInfoName i =
  case Info.lookup kNameInfo i of
    Just NameInfo {..} -> Just _infoName
    Nothing -> Nothing

setInfoName :: Name -> Info -> Info
setInfoName = Info.insert . NameInfo
