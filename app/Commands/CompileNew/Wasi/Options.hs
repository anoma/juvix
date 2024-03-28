module Commands.CompileNew.Wasi.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data WasiOptions = WasiOptions
  { _wasiCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseWasi :: Parser WasiOptions
parseWasi = do
  _wasiCompileCommonOptions <- parseCompileCommonOptions
  pure WasiOptions {..}
