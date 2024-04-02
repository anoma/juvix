module Commands.CompileNew.Anoma.Options (
module Commands.CompileNew.Anoma.Options ,

module Commands.CompileNew.CommonOptions,
                                         ) where

import Commands.CompileNew.CommonOptions
import CommonOptions

data AnomaOptions = AnomaOptions
  { _anomaCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''AnomaOptions

parseAnoma :: Parser AnomaOptions
parseAnoma = do
  _anomaCompileCommonOptions <- parseCompileCommonOptionsMain
  pure AnomaOptions {..}
