module Commands.Dev.Casm.Run.Options where

import CommonOptions

data CasmRunOptions = CasmRunOptions
  { _casmRunInputFile :: AppPath File,
    _casmRunDataFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''CasmRunOptions

parseCasmRunOptions :: Parser CasmRunOptions
parseCasmRunOptions = do
  _casmRunInputFile <- parseInputFile FileExtCasm
  _casmRunDataFile <- optional (parseProgramInputFile)
  pure CasmRunOptions {..}
