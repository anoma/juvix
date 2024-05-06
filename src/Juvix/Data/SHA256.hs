module Juvix.Data.SHA256 where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as Base16
import Juvix.Prelude

digestText :: Text -> Text
digestText =
  decodeUtf8Lenient
    . Base16.encode
    . SHA256.hash
    . encodeUtf8

-- | Create a HEX encoded, SHA256 digest of the contents of a file.
digestFile :: (Member Files r) => Path Abs File -> Sem r Text
digestFile = fmap (decodeUtf8Lenient . Base16.encode . SHA256.hash) . readFileBS'
