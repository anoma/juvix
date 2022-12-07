module Commands.Dev.Asm.Validate.Options where

import CommonOptions

newtype AsmValidateOptions = AsmValidateOptions
  { _asmValidateInputFile :: AppPath File
  }

makeLenses ''AsmValidateOptions

parseAsmValidateOptions :: Parser AsmValidateOptions
parseAsmValidateOptions = do
  _asmValidateInputFile <- parseInputJuvixAsmFile
  pure AsmValidateOptions {..}
