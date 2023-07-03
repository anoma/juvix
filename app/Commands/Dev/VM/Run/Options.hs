module Commands.Dev.VM.Run.Options where

import CommonOptions

newtype VMRunOptions = VMRunOptions
  { _vmRunInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''VMRunOptions

parseVMRunOptions :: Parser VMRunOptions
parseVMRunOptions = do
  _vmRunInputFile <- parseInputJuvixVMFile
  pure VMRunOptions {..}
