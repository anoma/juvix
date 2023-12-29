module Commands.Dev.Nockma.FromAsm.Options where

import CommonOptions

newtype NockmaFromAsmOptions = NockmaFromAsmOptions
  { _nockmaFromAsmOptionsAsmFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''NockmaFromAsmOptions

parseNockmaFromAsmOptions :: Parser NockmaFromAsmOptions
parseNockmaFromAsmOptions = do
  _nockmaFromAsmOptionsAsmFile <- parseInputFile FileExtJuvixAsm
  pure NockmaFromAsmOptions {..}
