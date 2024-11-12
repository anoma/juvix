module Commands.Dev.Nockma.Ide.Check.Options where

import CommonOptions

newtype NockmaCheckOptions = NockmaCheckOptions
  { _nockmaCheckFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''NockmaCheckOptions

parseNockmaCheckOptions :: Parser NockmaCheckOptions
parseNockmaCheckOptions = do
  _nockmaCheckFile <- parseInputFile FileExtNockma
  pure NockmaCheckOptions {..}
