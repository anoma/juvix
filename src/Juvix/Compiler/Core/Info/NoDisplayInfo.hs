module Juvix.Compiler.Core.Info.NoDisplayInfo where

import Juvix.Compiler.Core.Language.Base

newtype NoDisplayInfo = NoDisplayInfo ()

instance IsInfo NoDisplayInfo

kNoDisplayInfo :: Key NoDisplayInfo
kNoDisplayInfo = Proxy
