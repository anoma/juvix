module Commands.CompileNew.Vampir.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data VampirOptions = VampirOptions
  { _vampirCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseVampir :: Parser VampirOptions
parseVampir = do
  _vampirCompileCommonOptions <- parseCompileCommonOptions
  pure VampirOptions {..}
