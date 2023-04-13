module Juvix.Compiler.Core.Info.ExpansionInfo where

import Juvix.Compiler.Core.Language.Base

newtype ExpansionInfo = ExpansionInfo ()

instance IsInfo ExpansionInfo

kExpansionInfo :: Key ExpansionInfo
kExpansionInfo = Proxy
