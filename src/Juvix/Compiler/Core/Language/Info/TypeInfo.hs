module Juvix.Compiler.Core.Language.Info.TypeInfo where

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Type

newtype TypeInfo = TypeInfo {_infoType :: Type}

instance IsInfo TypeInfo

kTypeInfo :: Key TypeInfo
kTypeInfo = Proxy

makeLenses ''TypeInfo
