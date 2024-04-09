module Commands.Dev.DevCompile.Asm.Options
  ( module Commands.Dev.DevCompile.Asm.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data AsmOptions = AsmOptions
  { _asmCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''AsmOptions

parseAsm :: Parser AsmOptions
parseAsm = do
  _asmCompileCommonOptions <- parseCompileCommonOptions
  pure AsmOptions {..}
