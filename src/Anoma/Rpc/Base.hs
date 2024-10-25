module Anoma.Rpc.Base where

import Juvix.Prelude
import Juvix.Prelude.Aeson

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

example :: RunNock
example =
  RunNock
    { _runNockJammedProgram = "jamjam",
      _runNockPrivateInputs = [NockInputText "input"],
      _runNockPublicInputs = [NockInputJammed "public"]
    }
