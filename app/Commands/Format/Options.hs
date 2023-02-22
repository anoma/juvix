module Commands.Format.Options where

import CommonOptions

data FormatOptions = FormatOptions
  { _formatInputFile :: Maybe (AppPath File),
    _formatCheck :: Bool
  }
  deriving stock (Data)

makeLenses ''FormatOptions

parseFormat :: Parser FormatOptions
parseFormat = do
  _formatInputFile <- optional parseInputJuvixFile
  _formatCheck <-
    switch
      ( long "check"
          <> help "Check that a file is formatted"
      )
  pure FormatOptions {..}
