module Juvix.Compiler.Core.Info.VolatilityInfo where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info

newtype VolatilityInfo = VolatilityInfo
  { _infoIsVolatile :: Bool
  }

instance IsInfo VolatilityInfo

kVolatilityInfo :: Key VolatilityInfo
kVolatilityInfo = Proxy

makeLenses ''VolatilityInfo

isVolatile :: Info -> Bool
isVolatile i =
  case Info.lookup kVolatilityInfo i of
    Just VolatilityInfo {..} -> _infoIsVolatile
    Nothing -> False
