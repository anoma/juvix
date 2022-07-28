module Commands.Internal.Doc where

import Juvix.Prelude hiding (Doc)
import Options.Applicative

data DocOptions = DocOptions
  { _docOutputDir :: FilePath,
    _docOpen :: Bool
  }

makeLenses ''DocOptions

parseDoc :: Parser DocOptions
parseDoc = do
  _docOutputDir <-
    option
      str
      ( long "output-dir"
          <> metavar "DIR"
          <> value "doc"
          <> showDefault
          <> help "html output directory"
          <> action "directory"
      )
  _docOpen <-
    switch
      ( long "open"
          <> help "open the documentation after generating it"
      )
  pure DocOptions {..}
