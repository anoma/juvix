module Juvix.Core.Language.Info.ArgsNumInfo where

import Juvix.Core.Language.Base

newtype ArgsNumInfo = ArgsNumInfo
  { _infoArgsNum :: Int
  }

instance IsInfo ArgsNumInfo

kArgsNumInfo :: Key ArgsNumInfo
kArgsNumInfo = Proxy

makeLenses ''ArgsNumInfo
