module Commands.Dev.Reg.Read.Options where

import CommonOptions

newtype RegReadOptions = RegReadOptions
  { _regReadInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''RegReadOptions

parseRegReadOptions :: Parser RegReadOptions
parseRegReadOptions = do
  _regReadInputFile <- parseInputFile FileExtJuvixAsm
  pure RegReadOptions {..}
