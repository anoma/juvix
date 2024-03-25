module Commands.Dev.DevCompile.Reg.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data RegOptions = RegOptions
  { _regCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseReg :: Parser RegOptions
parseReg = do
  _regCompileCommonOptions <- parseCompileCommonOptions
  pure RegOptions {..}
