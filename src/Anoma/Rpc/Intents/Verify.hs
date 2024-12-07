module Anoma.Rpc.Intents.Verify where

import Anoma.Rpc.Base
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

verifyGrpcUrl :: GrpcMethodUrl
verifyGrpcUrl =
  mkGrpcMethodUrl $
    "Anoma" :| ["Protobuf", "IntentsService", "Verify"]

newtype Intent = Intent
  {_intentIntent :: Text}

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_intentIntent" -> "intent"
           _ -> impossibleError "All fields must be covered"
       }
     ''Intent
 )

data Request = Request
  { _requestNodeInfo :: NodeInfo,
    _requestIntent :: Intent
  }

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_requestNodeInfo" -> "node_info"
           "_requestIntent" -> "intent"
           _ -> impossibleError "All fields must be covered"
       }
     ''Request
 )

newtype Response = Response
  {_responseValid :: Bool}

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_responseValid" -> "valid"
           _ -> impossibleError "All fields must be covered"
       }
     ''Response
 )

makeLenses ''Request
makeLenses ''Response
