module Anoma.Http.AddTransaction.JsonOptions where

import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

addTransactionErrorOptions :: Aeson.Options
addTransactionErrorOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "_errorError" -> "error"
        _ -> impossibleError "All fields must be covered"
    }

addTransactionSuccessOptions :: Aeson.Options
addTransactionSuccessOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "_successMessage" -> "message"
        _ -> impossibleError "All fields must be covered"
    }
