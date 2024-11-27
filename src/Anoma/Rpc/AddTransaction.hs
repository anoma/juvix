module Anoma.Rpc.AddTransaction where

import Anoma.Rpc.Base
import Juvix.Prelude
import Juvix.Prelude.Aeson

addTransactionGrpcUrl :: GrpcMethodUrl
addTransactionGrpcUrl =
  mkGrpcMethodUrl $
    "Anoma" :| ["Protobuf", "MempoolService", "Add"]

data AddTransaction = AddTransaction
  { _addTransactionTransaction :: Text,
    _addTransactionNodeInfo :: NodeInfo
  }

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_addTransactionTransaction" -> "transaction"
           "_addTransactionNodeInfo" -> "node_info"
           _ -> impossibleError "All fields must be covered"
       }
     ''AddTransaction
 )

makeLenses ''AddTransaction
