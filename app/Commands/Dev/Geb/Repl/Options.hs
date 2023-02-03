module Commands.Dev.Geb.Repl.Options where

import CommonOptions

data GebReplOptions = GebReplOptions
  { _gebReplOptionsSilent :: Bool,
    _gebReplOptionsNoColors :: Bool
  }
  deriving stock (Data)

makeLenses ''GebReplOptions

defaultGebReplOptions :: GebReplOptions
defaultGebReplOptions =
  GebReplOptions
    { _gebReplOptionsSilent = False,
      _gebReplOptionsNoColors = False
    }

parseGebReplOptions :: Parser GebReplOptions
parseGebReplOptions = do
  _gebReplOptionsSilent <-
    switch
      ( long "--silent"
          <> help "Don't show the Juvix information in the REPL"
      )
  _gebReplOptionsNoColors <-
    switch
      ( long "no-colors"
          <> help "Don't use colors in the REPL"
      )
  pure GebReplOptions {..}
