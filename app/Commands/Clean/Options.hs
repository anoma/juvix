module Commands.Clean.Options where

import CommonOptions
import Juvix.Extra.Version

data CleanOptions = CleanOptions
  { _cleanOptionsGlobal :: Bool,
    _cleanOptionsOnlyGlobal :: Bool
  }
  deriving stock (Data)

makeLenses ''CleanOptions

parseCleanOptions :: Parser CleanOptions
parseCleanOptions = do
  _cleanOptionsGlobal <-
    switch
      ( long "global"
          <> short 'g'
          <> help ("Remove also $XDG_CONFIG_HOME/juvix/" <> unpack versionDoc)
      )
  _cleanOptionsOnlyGlobal <-
    switch
      ( long "global-only"
          <> help ("Remove only $XDG_CONFIG_HOME/juvix/" <> unpack versionDoc)
      )
  pure CleanOptions {..}
