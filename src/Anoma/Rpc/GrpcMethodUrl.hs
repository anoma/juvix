module Anoma.Rpc.GrpcMethodUrl
  ( GrpcMethodUrl,
    mkGrpcMethodUrl,
    grpcMethodUrlToText,
  )
where

import Data.Text qualified as Text
import Juvix.Prelude
import Prelude (show)

newtype GrpcMethodUrl = GrpcMethodUrl
  { _grpcMethodUrl :: NonEmpty Text
  }

mkGrpcMethodUrl :: NonEmpty Text -> GrpcMethodUrl
mkGrpcMethodUrl = GrpcMethodUrl

grpcMethodUrlToText :: GrpcMethodUrl -> Text
grpcMethodUrlToText (GrpcMethodUrl u) = Text.intercalate "." (toList u)

instance Show GrpcMethodUrl where
  show = unpack . grpcMethodUrlToText
