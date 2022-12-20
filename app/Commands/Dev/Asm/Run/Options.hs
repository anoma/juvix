module Commands.Dev.Asm.Run.Options where

import CommonOptions

data AsmRunOptions = AsmRunOptions
  { _asmRunNoValidate :: Bool,
    _asmRunInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''AsmRunOptions

parseAsmRunOptions :: Parser AsmRunOptions
parseAsmRunOptions = do
  _asmRunNoValidate <-
    switch
      ( long "no-validate"
          <> help "Don't validate the input before running"
      )
  _asmRunInputFile <- parseInputJuvixAsmFile
  pure AsmRunOptions {..}
