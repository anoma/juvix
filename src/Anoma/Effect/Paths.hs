module Anoma.Effect.Paths where

import Juvix.Prelude

clientRelFile :: Path Rel File
clientRelFile = $(mkRelFile "apps/anoma_client/anoma_client")

relProtoDir :: Path Rel Dir
relProtoDir = $(mkRelDir "apps/anoma_protobuf/priv/protobuf")

relProtoFile :: Path Rel File
relProtoFile = $(mkRelFile "anoma.proto")
