module Commands.Dev.Asm.Run.Options where

import CommonOptions

newtype AsmRunOptions = AsmRunOptions
  { _asmRunInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''AsmRunOptions

parseAsmRunOptions :: Parser AsmRunOptions
parseAsmRunOptions = do
  _asmRunInputFile <- parseInputJuvixFile
  pure AsmRunOptions {..}
