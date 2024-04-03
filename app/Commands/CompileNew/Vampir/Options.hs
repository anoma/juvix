module Commands.CompileNew.Vampir.Options
  ( module Commands.CompileNew.Vampir.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data VampirOptions = VampirOptions
  { _vampirCompileCommonOptions :: CompileCommonOptionsMain,
    _vampirUnsafe :: Bool
  }
  deriving stock (Data)

makeLenses ''VampirOptions

parseVampir :: Parser VampirOptions
parseVampir = do
  _vampirCompileCommonOptions <- parseCompileCommonOptionsMain
  _vampirUnsafe <-
    switch
      ( long "unsafe"
          <> help "Disable range and error checking (for targets: vampir)"
      )
  pure VampirOptions {..}
