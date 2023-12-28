module Commands.Dev.Nockma.FromAsm.Options where

import CommonOptions

newtype NockmaFromAsmOptions = NockmaFromAsmOptions
  { _nockmaFromAsmOptionsStackFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

parseNockmaFromAsmOptions :: Parser NockmaFromAsmOptions
parseNockmaFromAsmOptions = do
  _nockmaFromAsmOptionsStackFile <- optional (parseInputFile FileExtJuvixAsm)
  pure NockmaFromAsmOptions {..}
