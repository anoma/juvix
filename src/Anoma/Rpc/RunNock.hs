module Anoma.Rpc.RunNock where

import Anoma.Rpc.Base
import Juvix.Prelude
import Juvix.Prelude.Aeson

runNockGrpcUrl :: GrpcMethodUrl
runNockGrpcUrl =
  mkGrpcMethodUrl $
    "Anoma" :| ["Protobuf", "Intents", "Prove"]

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

newtype Response = Response
  { _proof :: Text
  }

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_proof" -> "proof"
           _ -> impossibleError "All fields must be covered"
       }
     ''Response
 )

makeLenses ''Response
