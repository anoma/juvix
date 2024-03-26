module Commands.Dev.DevCompile.Casm.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data CasmOptions = CasmOptions
  { _casmCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseCasm :: Parser CasmOptions
parseCasm = do
  _casmCompileCommonOptions <- parseCompileCommonOptions
  pure CasmOptions {..}
