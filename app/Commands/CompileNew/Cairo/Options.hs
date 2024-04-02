module Commands.CompileNew.Cairo.Options
  ( module Commands.CompileNew.Cairo.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data CairoOptions = CairoOptions
  { _cairoCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''CairoOptions

parseCairo :: Parser CairoOptions
parseCairo = do
  _cairoCompileCommonOptions <- parseCompileCommonOptionsMain
  pure CairoOptions {..}
