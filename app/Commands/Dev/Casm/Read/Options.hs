module Commands.Dev.Casm.Read.Options where

import CommonOptions
import Juvix.Compiler.Casm.Data.TransformationId

data CasmReadOptions = CasmReadOptions
  { _casmReadTransformations :: [TransformationId],
    _casmReadRun :: Bool,
    _casmReadNoPrint :: Bool,
    _casmReadInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''CasmReadOptions

parseCasmReadOptions :: Parser CasmReadOptions
parseCasmReadOptions = do
  _casmReadNoPrint <- optReadNoPrint
  _casmReadRun <- optReadRun
  _casmReadTransformations <- optCasmTransformationIds
  _casmReadInputFile <- parseInputFile FileExtCasm
  pure CasmReadOptions {..}
