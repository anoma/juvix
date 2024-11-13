module Commands.Dev.Nockma.Ide.Highlight.Options where

import CommonOptions

newtype NockmaHighlightOptions = NockmaHighlightOptions
  { _nockmaHighlightFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''NockmaHighlightOptions

parseNockmaHighlightOptions :: Parser NockmaHighlightOptions
parseNockmaHighlightOptions = do
  _nockmaHighlightFile <- parseInputFile FileExtNockma
  pure NockmaHighlightOptions {..}
