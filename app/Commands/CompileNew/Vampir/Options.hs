module Commands.CompileNew.Vampir.Options
  ( module Commands.CompileNew.Vampir.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data VampirOptions = VampirOptions
  { _vampirCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''VampirOptions

parseVampir :: Parser VampirOptions
parseVampir = do
  _vampirCompileCommonOptions <- parseCompileCommonOptionsMain
  pure VampirOptions {..}
