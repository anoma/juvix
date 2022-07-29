module Juvix.Core.Language.Info.LocationInfo where

import Juvix.Core.Prelude

newtype LocationInfo = LocationInfo {_infoLocation :: Location}

instance IsInfo LocationInfo

kLocationInfo :: Key LocationInfo
kLocationInfo = Proxy

makeLenses ''LocationInfo
