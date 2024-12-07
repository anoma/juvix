module Anoma.Rpc.Indexer.ListUnrevealedCommits where

import Anoma.Rpc.Base
import Anoma.Rpc.Indexer.ListUnrevealedCommits.JsonOptions
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

listUnrevealedCommitsGrpcUrl :: GrpcMethodUrl
listUnrevealedCommitsGrpcUrl =
  mkGrpcMethodUrl $
    "Anoma" :| ["Protobuf", "IndexerService", "ListUnrevealedCommits"]

newtype Request = Request
  {_requestNodeInfo :: NodeInfo}

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "_requestNodeInfo" -> "node_info"
           _ -> impossibleError "All fields must be covered"
       }
     ''Request
 )

newtype Response = Response
  {_responseCommits :: [Text]}

$(deriveToJSON responseOptions ''Response)

instance FromJSON Response where
  parseJSON =
    $(mkParseJSON responseOptions ''Response)
      . addDefaultValues' defaultValues
    where
      defaultValues :: HashMap Key Value
      defaultValues = hashMap [("commits", Aeson.Array mempty)]

makeLenses ''Request
makeLenses ''Response
