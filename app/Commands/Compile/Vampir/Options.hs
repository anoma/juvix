module Commands.Compile.Vampir.Options
  ( module Commands.Compile.Vampir.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data VampirOptions = VampirOptions
  { _vampirCompileCommonOptions :: CompileCommonOptionsMain,
    _vampirUnsafe :: Bool
  }
  deriving stock (Data)

makeLenses ''VampirOptions

parseVampir :: Parser VampirOptions
parseVampir = do
  _vampirCompileCommonOptions <- parseCompileCommonOptionsJuvixMain
  _vampirUnsafe <-
    switch
      ( long "unsafe"
          <> help "Disable range and error checking (for targets: vampir)"
      )
  pure VampirOptions {..}
