module Anoma.Http.Indexer.ListUnrevealedCommits where

import Anoma.Http.Base
import Anoma.Http.Indexer.ListUnrevealedCommits.JsonOptions
import Juvix.Prelude
import Juvix.Prelude.Aeson as Aeson

listUnrevealedCommitsUrl :: EndpointUrl
listUnrevealedCommitsUrl =
  mkEndpointUrl
    $ "indexer"
    :| ["unrevealed-commits"]

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

makeLenses ''Response
