-- | Options needed to derive JSON instances need to be put in a separate file due to
-- Template Haskell stage restriction
module Anoma.Http.RunNock.JsonOptions where

import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

nockErrorOptions :: Aeson.Options
nockErrorOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "_errorError" -> "error"
        "_errorTraces" -> "output"
        _ -> impossibleError "All fields must be covered"
    }

nockSuccessOptions :: Aeson.Options
nockSuccessOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "_successResult" -> "result"
        "_successTraces" -> "output"
        _ -> impossibleError "All fields must be covered"
    }
