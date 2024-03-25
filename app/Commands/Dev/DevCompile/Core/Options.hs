module Commands.Dev.DevCompile.Core.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data CoreOptions = CoreOptions
  { _coreCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseCore :: Parser CoreOptions
parseCore = do
  _coreCompileCommonOptions <- parseCompileCommonOptions
  pure CoreOptions {..}
