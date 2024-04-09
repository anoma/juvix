module Commands.Dev.DevCompile.Core.Options
  ( module Commands.Dev.DevCompile.Core.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data CoreOptions = CoreOptions
  { _coreCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''CoreOptions

parseCore :: Parser CoreOptions
parseCore = do
  _coreCompileCommonOptions <- parseCompileCommonOptionsJuvixMain
  pure CoreOptions {..}
