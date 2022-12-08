module Juvix.Compiler.Core.Info.LocationInfo where

import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language.Base

newtype LocationInfo = LocationInfo {_infoLocation :: Location}

instance IsInfo LocationInfo

kLocationInfo :: Key LocationInfo
kLocationInfo = Proxy

makeLenses ''LocationInfo

getInfoLocation :: Info -> Maybe Location
getInfoLocation i =
  case Info.lookup kLocationInfo i of
    Just LocationInfo {..} -> Just _infoLocation
    Nothing -> Nothing

setInfoLocation :: Location -> Info -> Info
setInfoLocation = Info.insert . LocationInfo
