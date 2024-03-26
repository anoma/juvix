module Commands.Dev.DevCompile.Asm.Options
  ( module Commands.Dev.DevCompile.Asm.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data AsmOptions = AsmOptions
  { _asmCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

makeLenses ''AsmOptions

parseAsm :: Parser AsmOptions
parseAsm = do
  _asmCompileCommonOptions <- parseCompileCommonOptions
  pure AsmOptions {..}
