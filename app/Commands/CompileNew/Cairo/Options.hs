module Commands.CompileNew.Cairo.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data CairoOptions = CairoOptions
  { _cairoCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseCairo :: Parser CairoOptions
parseCairo = do
  _cairoCompileCommonOptions <- parseCompileCommonOptions
  pure CairoOptions {..}
