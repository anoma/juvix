module Juvix.Core.Info.LocationInfo where

import Juvix.Core.Prelude

newtype LocationInfo = LocationInfo {_infoLocation :: Location}

kLocationInfo :: Key LocationInfo
kLocationInfo = Proxy

makeLenses ''LocationInfo
