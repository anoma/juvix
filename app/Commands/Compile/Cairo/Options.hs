module Commands.Compile.Cairo.Options
  ( module Commands.Compile.Cairo.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data CairoOptions = CairoOptions
  { _cairoCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''CairoOptions

parseCairo :: Parser CairoOptions
parseCairo = do
  _cairoCompileCommonOptions <- parseCompileCommonOptionsJuvixMain
  pure CairoOptions {..}
