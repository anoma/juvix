module Commands.Clean.Options where

import CommonOptions
import Juvix.Extra.Version

newtype CleanOptions = CleanOptions
  {_cleanOptionsGlobal :: Bool}
  deriving stock (Data)

makeLenses ''CleanOptions

parseCleanOptions :: Parser CleanOptions
parseCleanOptions = do
  _cleanOptionsGlobal <-
    switch
      ( long "global"
          <> short 'g'
          <> help ("Remove $XDG_CONFIG_HOME/juvix/" <> unpack versionDoc)
      )
  pure CleanOptions {..}
