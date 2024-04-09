module Commands.Compile.Anoma.Options
  ( module Commands.Compile.Anoma.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data AnomaOptions = AnomaOptions
  { _anomaCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''AnomaOptions

parseAnoma :: Parser AnomaOptions
parseAnoma = do
  _anomaCompileCommonOptions <- parseCompileCommonOptions
  pure AnomaOptions {..}
