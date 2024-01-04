module Commands.Dev.Casm.Read.Options where

import CommonOptions

newtype CasmReadOptions = CasmReadOptions
  { _casmReadInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''CasmReadOptions

parseCasmReadOptions :: Parser CasmReadOptions
parseCasmReadOptions = do
  _casmReadInputFile <- parseInputFile FileExtCasm
  pure CasmReadOptions {..}
