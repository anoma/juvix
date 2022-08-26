module Juvix.Compiler.Core.Info.LocationInfo where

import Juvix.Compiler.Core.Language.Base

newtype LocationInfo = LocationInfo {_infoLocation :: Location}

instance IsInfo LocationInfo

kLocationInfo :: Key LocationInfo
kLocationInfo = Proxy

makeLenses ''LocationInfo
