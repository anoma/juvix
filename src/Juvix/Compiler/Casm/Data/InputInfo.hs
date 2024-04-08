module Juvix.Compiler.Casm.Data.InputInfo where

import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.HashMap.Strict qualified as HashMap
import Data.Scientific
import Juvix.Data.Field
import Juvix.Prelude

newtype InputInfo = InputInfo
  { _inputInfoMap :: HashMap Text FField
  }
  deriving stock (Generic, Show)

makeLenses ''InputInfo

instance FromJSON InputInfo where
  parseJSON = \case
    Object obj -> do
      lst <-
        forM (KeyMap.toList obj) $ \(k, v) -> do
          v' <- parseFField v
          return (toText k, v')
      return
        . InputInfo
        . HashMap.fromList
        $ lst
    v -> typeMismatch "Object" v
    where
      parseFField :: Value -> Parser FField
      parseFField = \case
        Number x
          | isInteger x ->
              return $ fieldFromInteger cairoFieldSize (fromRight 0 $ floatingOrInteger @Double x)
        v ->
          typeMismatch "Integer" v
