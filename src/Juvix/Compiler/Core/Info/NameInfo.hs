module Juvix.Compiler.Core.Info.NameInfo where

import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language.Base

newtype NameInfo = NameInfo {_infoName :: Text}

instance IsInfo NameInfo

kNameInfo :: Key NameInfo
kNameInfo = Proxy

makeLenses ''NameInfo

getInfoName :: Info -> Text
getInfoName i =
  case Info.lookup kNameInfo i of
    Just NameInfo {..} -> _infoName
    Nothing -> ""

setInfoName :: Text -> Info -> Info
setInfoName = Info.insert . NameInfo
