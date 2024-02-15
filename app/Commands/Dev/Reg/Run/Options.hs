module Commands.Dev.Reg.Run.Options where

import CommonOptions

newtype RegRunOptions = RegRunOptions
  { _regRunInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''RegRunOptions

parseRegRunOptions :: Parser RegRunOptions
parseRegRunOptions = do
  _regRunInputFile <- parseInputFile FileExtJuvixReg
  pure RegRunOptions {..}
