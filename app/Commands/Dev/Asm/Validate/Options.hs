module Commands.Dev.Asm.Validate.Options where

import CommonOptions

data AsmValidateOptions = AsmValidateOptions
  { _asmValidateInputFile :: Path,
    _asmValidateNoPrint :: Bool
  }
  deriving stock (Data)

makeLenses ''AsmValidateOptions

parseAsmValidateOptions :: Parser AsmValidateOptions
parseAsmValidateOptions = do
  _asmValidateInputFile <- parseInputJuvixAsmFile
  _asmValidateNoPrint <-
    switch
      ( long "no-print"
          <> help "Don't pretty print the file"
      )
  pure AsmValidateOptions {..}
