module Commands.CompileNew.Casm.Options where

import CommonOptions

data CasmOptions
  deriving stock (Data)

parseCasm :: Parser CasmOptions
parseCasm = undefined
