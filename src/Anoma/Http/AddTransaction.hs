module Anoma.Http.AddTransaction where

import Anoma.Http.AddTransaction.JsonOptions
import Anoma.Http.Base
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

addTransactionUrl :: EndpointUrl
addTransactionUrl =
  mkEndpointUrl $
    "mempool" :| ["add"]

data TransactionType
  = TransactionTransparent
  | TransactionCairo

data AddTransaction = AddTransaction
  { _addTransactionTransaction :: Text,
    _addTransactionType :: TransactionType
  }

$( deriveJSON
     defaultOptions
       { constructorTagModifier = \case
           "TransactionTransparent" -> "transparent_resource"
           "TransactionCairo" -> "cairo"
           _ -> impossibleError "All fields must be covered"
       }
     ''TransactionType
 )

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_addTransactionTransaction" -> "transaction"
           "_addTransactionType" -> "transaction_type"
           _ -> impossibleError "All fields must be covered"
       }
     ''AddTransaction
 )

newtype AddTransactionError = AddTransactionError
  { _errorError :: Text
  }

$(deriveToJSON addTransactionErrorOptions ''AddTransactionError)

instance FromJSON AddTransactionError where
  parseJSON =
    $(mkParseJSON addTransactionErrorOptions ''AddTransactionError)
      . addDefaultValues' defaultValues
    where
      defaultValues :: HashMap Key Value
      defaultValues = hashMap [("error", Aeson.Array mempty)]

newtype AddTransactionSuccess = AddTransactionSuccess
  { _successMessage :: Text
  }

$(deriveToJSON addTransactionSuccessOptions ''AddTransactionSuccess)

instance FromJSON AddTransactionSuccess where
  parseJSON =
    $(mkParseJSON addTransactionSuccessOptions ''AddTransactionSuccess)
      . addDefaultValues' defaultValues
    where
      defaultValues :: HashMap Key Value
      defaultValues = hashMap [("message", Aeson.Array mempty)]

data Response
  = ResponseSuccess AddTransactionSuccess
  | ResponseError AddTransactionError

$( deriveJSON
     defaultOptions
       { unwrapUnaryRecords = True,
         sumEncoding = ObjectWithSingleField,
         constructorTagModifier = \case
           "ResponseSuccess" -> "success"
           "ResponseError" -> "error"
           _ -> impossibleError "All constructors must be covered"
       }
     ''Response
 )

makeLenses ''Response
makeLenses ''AddTransactionSuccess
makeLenses ''AddTransactionError
makeLenses ''AddTransaction
