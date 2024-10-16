module Juvix.Config where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed qualified as FE
import Juvix.Prelude

data Config = Config
  { _configWasm :: Bool,
    _configRust :: Bool,
    _configClang :: Text,
    _configCargo :: Text
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

emptyConfig :: Config
emptyConfig =
  Config
    { _configWasm = False,
      _configRust = False,
      _configClang = "",
      _configCargo = ""
    }

config :: Config
config =
  fromMaybe emptyConfig $
    decode $
      BL.fromStrict $(FE.makeRelativeToProject "config/config.json" >>= FE.embedFile)
