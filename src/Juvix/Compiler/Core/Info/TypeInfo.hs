module Juvix.Compiler.Core.Info.TypeInfo where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Info
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

newtype TypeInfo = TypeInfo
  { _infoType :: Type
  }

instance IsInfo TypeInfo

kTypeInfo :: Key TypeInfo
kTypeInfo = Proxy

makeLenses ''TypeInfo

getInfoType :: Info -> Type
getInfoType i =
  case Info.lookup kTypeInfo i of
    Just TypeInfo {..} -> _infoType
    Nothing -> mkDynamic'

setInfoType :: Type -> Info -> Info
setInfoType = Info.insert . TypeInfo

getNodeType :: Node -> Type
getNodeType = getInfoType . getInfo

setNodeType :: Type -> Node -> Node
setNodeType = modifyInfo . setInfoType

removeTypeInfo :: Node -> Node
removeTypeInfo = removeInfo kTypeInfo
