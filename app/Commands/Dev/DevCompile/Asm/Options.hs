module Commands.Dev.DevCompile.Asm.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data AsmOptions = AsmOptions
  { _asmCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseAsm :: Parser AsmOptions
parseAsm = do
  _asmCompileCommonOptions <- parseCompileCommonOptions
  pure AsmOptions {..}
