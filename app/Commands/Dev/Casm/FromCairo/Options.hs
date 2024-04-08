module Commands.Dev.Casm.FromCairo.Options where

import CommonOptions

newtype CasmFromCairoOptions = CasmFromCairoOptions
  { _casmFromCairoInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''CasmFromCairoOptions

parseCasmFromCairoOptions :: Parser CasmFromCairoOptions
parseCasmFromCairoOptions = do
  _casmFromCairoInputFile <- parseInputFile FileExtJson
  pure CasmFromCairoOptions {..}
