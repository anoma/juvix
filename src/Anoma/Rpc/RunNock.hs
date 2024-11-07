module Anoma.Rpc.RunNock where

import Anoma.Rpc.Base
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

runNockGrpcUrl :: GrpcMethodUrl
runNockGrpcUrl =
  mkGrpcMethodUrl $
    "Anoma" :| ["Protobuf", "NockService", "Prove"]

data NockInput
  = NockInputText Text
  | NockInputJammed Text

$( deriveJSON
     defaultOptions
       { constructorTagModifier = \case
           "NockInputText" -> "text"
           "NockInputJammed" -> "jammed"
           _ -> impossibleError "All constructors must be covered",
         sumEncoding = ObjectWithSingleField
       }
     ''NockInput
 )

data RunNock = RunNock
  { _runNockJammedProgram :: Text,
    _runNockPrivateInputs :: [NockInput],
    _runNockPublicInputs :: [NockInput]
  }

$( deriveJSON
     defaultOptions
       { unwrapUnaryRecords = True,
         fieldLabelModifier = \case
           "_runNockJammedProgram" -> "jammed_program"
           "_runNockPrivateInputs" -> "private_inputs"
           "_runNockPublicInputs" -> "public_inputs"
           _ -> impossibleError "All fields must be covered"
       }
     ''RunNock
 )

data NockSuccess = NockSuccess
  { _successResult :: Text,
    _successTraces :: [Text]
  }

$( deriveToJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_successResult" -> "result"
           "_successTraces" -> "output"
           _ -> impossibleError "All fields must be covered"
       }
     ''NockSuccess
 )

instance FromJSON NockSuccess where
  parseJSON =
    $( mkParseJSON
         defaultOptions
           { fieldLabelModifier = \case
               "_successResult" -> "result"
               "_successTraces" -> "output"
               _ -> impossibleError "All fields must be covered"
           }
         ''NockSuccess
     )
      . addDefaultValues' defaultValues
    where
      defaultValues :: HashMap Key Value
      defaultValues = hashMap [("output", Aeson.Array mempty)]

data Response
  = ResponseSuccess NockSuccess
  | ResponseError Text

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
