module Commands.Dev.DevCompile.Nockma.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data NockmaOptions = NockmaOptions
  { _nockmaCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseNockma :: Parser NockmaOptions
parseNockma = do
  _nockmaCompileCommonOptions <- parseCompileCommonOptions
  pure NockmaOptions {..}
