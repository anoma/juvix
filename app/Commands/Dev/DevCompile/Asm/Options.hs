module Commands.Dev.DevCompile.Asm.Options
  ( module Commands.Dev.DevCompile.Asm.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data AsmOptions = AsmOptions
  { _asmCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''AsmOptions

parseAsm :: Parser AsmOptions
parseAsm = do
  _asmCompileCommonOptions <- parseCompileCommonOptionsMain
  pure AsmOptions {..}
