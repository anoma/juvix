module Commands.Dev.Anoma.Intents.Verify.Options where

import CommonOptions

newtype IntentsVerifyOptions = IntentsVerifyOptions
  {_intentsVerifyFile :: AppPath File}
  deriving stock (Data)

makeLenses ''IntentsVerifyOptions

parseIntentsVerifyOptions :: Parser IntentsVerifyOptions
parseIntentsVerifyOptions = do
  _intentsVerifyFile <- parseInputFile FileExtNockma
  pure IntentsVerifyOptions {..}
