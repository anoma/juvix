module Commands.Dev.DevCompile.Reg.Options
  ( module Commands.Dev.DevCompile.Reg.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data RegOptions = RegOptions
  { _regCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''RegOptions

parseReg :: Parser RegOptions
parseReg = do
  _regCompileCommonOptions <- parseCompileCommonOptions
  pure RegOptions {..}
