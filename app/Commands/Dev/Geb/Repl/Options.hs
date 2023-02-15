module Commands.Dev.Geb.Repl.Options where

import CommonOptions

newtype GebReplOptions = GebReplOptions
  { _gebReplOptionsSilent :: Bool
  }
  deriving stock (Data)

makeLenses ''GebReplOptions

defaultGebReplOptions :: GebReplOptions
defaultGebReplOptions =
  GebReplOptions
    { _gebReplOptionsSilent = False
    }

parseGebReplOptions :: Parser GebReplOptions
parseGebReplOptions = do
  _gebReplOptionsSilent <-
    switch
      ( long "--silent"
          <> help "Don't show the Juvix information in the REPL"
      )
  pure GebReplOptions {..}
