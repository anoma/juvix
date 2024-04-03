module Commands.Dev.DevCompile.Reg.Options
  ( module Commands.Dev.DevCompile.Reg.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data RegOptions = RegOptions
  { _regCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''RegOptions

parseReg :: Parser RegOptions
parseReg = do
  _regCompileCommonOptions <- parseCompileCommonOptionsMain
  pure RegOptions {..}