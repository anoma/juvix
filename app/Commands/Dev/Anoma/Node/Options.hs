module Commands.Dev.Anoma.Node.Options where

import CommonOptions

newtype NodeOptions = NodeOptions
  { _nodeAnomaPath :: AppPath Dir
  }
  deriving stock (Data)

makeLenses ''NodeOptions

parseNodeOptions :: Parser NodeOptions
parseNodeOptions = do
  _nodeAnomaPath <-
    parseGenericOutputDir
      ( help "Anoma repository directory"
          <> action "directory"
      )

  pure NodeOptions {..}
