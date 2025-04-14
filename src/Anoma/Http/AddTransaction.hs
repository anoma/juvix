module Anoma.Http.AddTransaction where

import Anoma.Http.Base
import Juvix.Prelude
import Juvix.Prelude.Aeson

addTransactionUrl :: EndpointUrl
addTransactionUrl =
  mkEndpointUrl $
    "mempool" :| ["add"]

data AddTransaction = AddTransaction
  { _addTransactionTransaction :: Text
  }

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_addTransactionTransaction" -> "transaction"
           _ -> impossibleError "All fields must be covered"
       }
     ''AddTransaction
 )

makeLenses ''AddTransaction
