module Commands.Dev.Nockma.Format.Options where

import CommonOptions

newtype NockmaFormatOptions = NockmaFormatOptions
  { _nockmaFormatFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''NockmaFormatOptions

parseNockmaFormatOptions :: Parser NockmaFormatOptions
parseNockmaFormatOptions = do
  _nockmaFormatFile <- parseInputFile FileExtNockma
  pure NockmaFormatOptions {..}
