module Commands.Dev.DevCompile.Casm.Options
  ( module Commands.Dev.DevCompile.Casm.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data CasmOptions = CasmOptions
  { _casmCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''CasmOptions

parseCasm :: Parser CasmOptions
parseCasm = do
  _casmCompileCommonOptions <- parseCompileCommonOptionsMain
  pure CasmOptions {..}