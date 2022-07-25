module Commands.Internal.Doc where

import Juvix.Prelude hiding (Doc)
import Options.Applicative

newtype DocOptions = DocOptions
  { _docOutputDir :: FilePath
  }

makeLenses ''DocOptions

parseDoc :: Parser DocOptions
parseDoc = do
  _docOutputDir <-
    option
      str
      ( long "output-dir"
          <> metavar "DIR"
          <> value "html"
          <> showDefault
          <> help "html output directory"
          <> action "directory"
      )
  pure DocOptions {..}
