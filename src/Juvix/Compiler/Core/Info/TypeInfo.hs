module Juvix.Compiler.Core.Info.TypeInfo where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Language

newtype TypeInfo = TypeInfo {_infoType :: Type}

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

removeTypeInfo :: Node -> Node
removeTypeInfo = removeInfo kTypeInfo

inferTypeInfo :: InfoTable -> Node -> Node
inferTypeInfo _ = umap id -- TODO: store the type of each node in its info
