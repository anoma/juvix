module Juvix.Config where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed qualified as FE
import Juvix.Prelude

data Config = Config
  { _configWasm :: Bool,
    _configRust :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = genericParseJSON opts
    where
      opts =
        defaultOptions
          { fieldLabelModifier = map toLower . dropPrefix "_config"
          }

config :: Config
config =
  fromMaybe (Config False False) $
    decode $
      BL.fromStrict $(FE.makeRelativeToProject "config/config.json" >>= FE.embedFile)
