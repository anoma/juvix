module Commands.Dev.PlainMarkdown.Format.Options where

import CommonOptions

data FormatOptions = FormatOptions
  { _formatFile :: AppPath File,
    _formatCheck :: Bool
  }
  deriving stock (Data)

makeLenses ''FormatOptions

parseFormatOptions :: Parser FormatOptions
parseFormatOptions = do
  _formatFile <- parseInputFile FileExtMarkdown
  _formatCheck <-
    switch
      ( long "check"
          <> help "Exit code 1 if a file wasn't already formatted."
      )

  pure FormatOptions {..}
