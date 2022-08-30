module Juvix.Compiler.Core.Info.TypeInfo where

import Juvix.Compiler.Core.Language

newtype TypeInfo = TypeInfo {_infoType :: Type}

instance IsInfo TypeInfo

kTypeInfo :: Key TypeInfo
kTypeInfo = Proxy

makeLenses ''TypeInfo
