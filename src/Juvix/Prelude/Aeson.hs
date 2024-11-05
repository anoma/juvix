module Juvix.Prelude.Aeson
  ( module Juvix.Prelude.Aeson,
    module Data.Aeson,
    module Data.Aeson.TH,
    module Data.Aeson.Text,
  )
where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH
import Data.Aeson.Text
import Data.ByteString.Lazy qualified as BS
import Data.Text.Lazy qualified as Lazy
import Juvix.Prelude.Base

readJSONFile :: (FromJSON a) => FilePath -> IO (Maybe a)
readJSONFile f = do
  bs <- BS.readFile f
  return $ decode bs

jsonEncodeToText :: (ToJSON a) => a -> Text
jsonEncodeToText = Lazy.toStrict . encodeToLazyText

jsonEncodeToPrettyText :: (ToJSON a) => a -> Text
jsonEncodeToPrettyText = decodeUtf8Lenient . BS.toStrict . encodePretty

jsonAppendFields :: [(Key, Value)] -> Value -> Value
jsonAppendFields keyValues = \case
  Object obj -> Object (KeyMap.fromList keyValues <> obj)
  a -> a
