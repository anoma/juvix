module Juvix.Prelude.Aeson
  ( module Juvix.Prelude.Aeson,
    module Data.Aeson,
    module Data.Aeson.Text,
  )
where

import Data.Aeson
import Data.Aeson.Text
import Data.Text.Lazy qualified as Lazy
import Juvix.Prelude.Base

encodeToText :: (ToJSON a) => a -> Text
encodeToText = Lazy.toStrict . encodeToLazyText
