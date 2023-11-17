module Juvix.Prelude.Aeson
  ( module Juvix.Prelude.Aeson,
    module Data.Aeson,
    module Data.Aeson.Text,
  )
where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text
import Data.Text.Lazy qualified as Lazy
import Juvix.Prelude.Base

encodeToText :: (ToJSON a) => a -> Text
encodeToText = Lazy.toStrict . encodeToLazyText

appendFields :: [(Key, Value)] -> Value -> Value
appendFields keyValues = \case
  Object obj -> Object (KeyMap.fromList keyValues <> obj)
  a -> a
