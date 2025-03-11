module Commands.Dev.PlainMarkdown.Format.Options where

import CommonOptions

newtype FormatOptions = FormatOptions
  { _formatFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''FormatOptions

parseFormatOptions :: Parser FormatOptions
parseFormatOptions = do
  _formatFile <- parseInputFile FileExtMarkdown
  pure FormatOptions {..}
