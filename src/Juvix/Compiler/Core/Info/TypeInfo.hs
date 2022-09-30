module Juvix.Compiler.Core.Info.TypeInfo where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

newtype TypeInfo = TypeInfo {_infoType :: Type}

instance IsInfo TypeInfo

kTypeInfo :: Key TypeInfo
kTypeInfo = Proxy

makeLenses ''TypeInfo

getInfoType :: Info -> Type
getInfoType i = maybe mkDynamic' (^. infoType) (Info.lookup kTypeInfo i)

setInfoType :: Type -> Info -> Info
setInfoType = Info.insert . TypeInfo
