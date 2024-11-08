module Juvix.Prelude.Aeson
  ( module Juvix.Prelude.Aeson,
    module Data.Aeson,
    module Data.Aeson.TH,
    module Data.Aeson.Text,
    module Data.Aeson.KeyMap,
  )
where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH
import Data.Aeson.Text
import Data.ByteString.Lazy qualified as BS
import Data.HashMap.Strict qualified as HashMap
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

addDefaultValues :: HashMap Key Value -> Object -> Object
addDefaultValues defVals obj = run . execState obj $
  forM_ (HashMap.toList defVals) $ \(k, def) -> do
    modify (insertWith (\_new old -> old) k def)

-- | Fails when the given Value is not an object
addDefaultValues' :: HashMap Key Value -> Value -> Value
addDefaultValues' defVals v = case v of
  Object obj -> Object (addDefaultValues defVals obj)
  _ -> error "Expected an object"
