module Juvix.Prelude.Aeson
  ( module Juvix.Prelude.Aeson,
    module Data.Aeson,
    module Data.Aeson.Text,
  )
where

import Data.Aeson
import Data.Aeson.BetterErrors
import Data.Aeson.Text
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Lazy qualified as Lazy
import Juvix.Prelude.Base

newtype JSONIntData = JSONIntData
  { _jsonIntDataMap :: HashMap Text Int
  }

makeLenses ''JSONIntData

type JSONError = Text

instance FromJSON JSONIntData where
  parseJSON = toAesonParser id parseData
    where
      parseData :: Parse JSONError JSONIntData
      parseData = do
        d <- eachInObject asIntegral
        let _jsonIntDataMap = HashMap.fromList d
        return JSONIntData {..}

encodeToText :: (ToJSON a) => a -> Text
encodeToText = Lazy.toStrict . encodeToLazyText
