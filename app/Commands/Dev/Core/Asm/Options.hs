module Commands.Dev.Core.Asm.Options where

import CommonOptions

data CoreAsmOptions = CoreAsmOptions
  { _coreAsmPrint :: Bool,
    _coreAsmInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''CoreAsmOptions

parseCoreAsmOptions :: Parser CoreAsmOptions
parseCoreAsmOptions = do
  _coreAsmPrint <-
    switch
      ( long "print"
          <> help "print the generated JuvixAsm code instead of running it"
      )
  _coreAsmInputFile <- parseInputJuvixCoreFile
  pure CoreAsmOptions {..}
