module Commands.CompileNew.Wasi.Options where

import CommonOptions

data WasiOptions
  deriving stock (Data)

parseWasi :: Parser WasiOptions
parseWasi = undefined
