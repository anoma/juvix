module Anoma.Http.RunNock where

import Anoma.Http.Base
import Anoma.Http.RunNock.JsonOptions
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

runNockUrl :: EndpointUrl
runNockUrl =
  mkEndpointUrl
    $ "nock"
    :| ["prove"]

data RunNock = RunNock
  { _runNockJammedProgram :: Text,
    _runNockPrivateInputs :: [Text],
    _runNockPublicInputs :: [Text]
  }

$( deriveJSON
     defaultOptions
       { unwrapUnaryRecords = True,
         fieldLabelModifier = \case
           "_runNockJammedProgram" -> "program"
           "_runNockPrivateInputs" -> "private_inputs"
           "_runNockPublicInputs" -> "public_inputs"
           _ -> impossibleError "All fields must be covered"
       }
     ''RunNock
 )

data NockError = NockError
  { _errorError :: Text,
    _errorTraces :: [Text]
  }

$(deriveToJSON nockErrorOptions ''NockError)

instance FromJSON NockError where
  parseJSON =
    $(mkParseJSON nockErrorOptions ''NockError)
      . addDefaultValues' defaultValues
    where
      defaultValues :: HashMap Key Value
      defaultValues = hashMap [("result", Aeson.Array mempty)]

data NockSuccess = NockSuccess
  { _successResult :: Text,
    _successTraces :: [Text]
  }

$(deriveToJSON nockSuccessOptions ''NockSuccess)

instance FromJSON NockSuccess where
  parseJSON =
    $(mkParseJSON nockSuccessOptions ''NockSuccess)
      . addDefaultValues' defaultValues
    where
      defaultValues :: HashMap Key Value
      defaultValues = hashMap [("result", Aeson.Array mempty)]

data Response
  = ResponseSuccess NockSuccess
  | ResponseError NockError

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
makeLenses ''NockSuccess
makeLenses ''NockError
