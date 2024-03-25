module Commands.CompileNew.Anoma.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data AnomaOptions = AnomaOptions
  { _anomaCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseAnoma :: Parser AnomaOptions
parseAnoma = do
  _anomaCompileCommonOptions <- parseCompileCommonOptions
  pure AnomaOptions {..}
