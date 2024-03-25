module Commands.CompileNew.Geb.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data GebOptions = GebOptions
  { _gebCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseGeb :: Parser GebOptions
parseGeb = do
  _gebCompileCommonOptions <- parseCompileCommonOptions
  pure GebOptions {..}
