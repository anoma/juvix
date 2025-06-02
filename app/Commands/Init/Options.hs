module Commands.Init.Options where

import CommonOptions

data InitOptions = InitOptions
  { _initOptionsInteractive :: Bool,
    _initOptionsBasic :: Bool
  }
  deriving stock (Data)

makeLenses ''InitOptions

parseInitOptions :: Parser InitOptions
parseInitOptions = do
  _initOptionsInteractive <-
    switch
      ( long "interactive"
          <> short 'i'
          <> help "Run interactively"
      )
  _initOptionsBasic <-
    switch
      ( long "basic"
          <> short 'b'
          <> help "Generate a basic Package.juvix that does not depend on the standard library"
      )
  pure InitOptions {..}
