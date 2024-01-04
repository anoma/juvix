module Commands.Dev.Casm.Run.Options where

import CommonOptions

newtype CasmRunOptions = CasmRunOptions
  { _casmRunInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''CasmRunOptions

parseCasmRunOptions :: Parser CasmRunOptions
parseCasmRunOptions = do
  _casmRunInputFile <- parseInputFile FileExtCasm
  pure CasmRunOptions {..}
