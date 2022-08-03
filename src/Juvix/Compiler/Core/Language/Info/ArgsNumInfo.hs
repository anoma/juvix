module Juvix.Compiler.Core.Language.Info.ArgsNumInfo where

import Juvix.Compiler.Core.Language.Base

newtype ArgsNumInfo = ArgsNumInfo
  { _infoArgsNum :: Int
  }

instance IsInfo ArgsNumInfo

kArgsNumInfo :: Key ArgsNumInfo
kArgsNumInfo = Proxy

makeLenses ''ArgsNumInfo
