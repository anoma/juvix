module Commands.Init.Options where

import CommonOptions

newtype InitOptions = InitOptions
  {_initOptionsNonInteractive :: Bool}
  deriving stock (Data)

makeLenses ''InitOptions

parseInitOptions :: Parser InitOptions
parseInitOptions = do
  _initOptionsNonInteractive <-
    switch
      ( long "non-interactive"
          <> short 'n'
          <> help "Run non-interactively. Generates a default Package.juvix"
      )
  pure InitOptions {..}
