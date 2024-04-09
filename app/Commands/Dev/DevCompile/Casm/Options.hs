module Commands.Dev.DevCompile.Casm.Options
  ( module Commands.Dev.DevCompile.Casm.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data CasmOptions = CasmOptions
  { _casmCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''CasmOptions

parseCasm :: Parser CasmOptions
parseCasm = do
  _casmCompileCommonOptions <- parseCompileCommonOptions
  pure CasmOptions {..}
