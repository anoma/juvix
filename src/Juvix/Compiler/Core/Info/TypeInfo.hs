module Juvix.Compiler.Core.Info.TypeInfo where

import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

newtype TypeInfo = TypeInfo {_infoType :: Type}

instance IsInfo TypeInfo

kTypeInfo :: Key TypeInfo
kTypeInfo = Proxy

makeLenses ''TypeInfo

getInfoType :: Info -> Maybe Type
getInfoType i =
  case Info.lookup kTypeInfo i of
    Just (TypeInfo {..}) -> Just _infoType
    Nothing -> Nothing

setInfoType :: Type -> Info -> Info
setInfoType = Info.insert . TypeInfo
