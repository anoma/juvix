module Commands.Init.Options where

import CommonOptions

data InitOptions = InitOptions
  { _initOptionsNonInteractive :: Bool,
    _initOptionsBasic :: Bool
  }
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
  _initOptionsBasic <-
    switch
      ( long "basic"
          <> short 'b'
          <> help "Run non-interactively. Generates a basic Package.juvix that does not depend on the standard library"
      )
  pure InitOptions {..}
