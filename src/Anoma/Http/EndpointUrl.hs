module Anoma.Http.EndpointUrl
  ( EndpointUrl,
    mkEndpointUrl,
    endpointUrlToText,
  )
where

import Data.Text qualified as Text
import Juvix.Prelude
import Prelude (show)

newtype EndpointUrl = EndpointUrl
  { _endpointUrl :: NonEmpty Text
  }

mkEndpointUrl :: NonEmpty Text -> EndpointUrl
mkEndpointUrl = EndpointUrl

endpointUrlToText :: EndpointUrl -> Text
endpointUrlToText (EndpointUrl u) = Text.intercalate "/" (toList u)

instance Show EndpointUrl where
  show = unpack . endpointUrlToText
