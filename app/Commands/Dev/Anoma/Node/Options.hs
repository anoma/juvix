module Commands.Dev.Anoma.Node.Options where

import CommonOptions

newtype NodeOptions = NodeOptions
  { _nodeAnomaPath :: AppPath Dir
  }
  deriving stock (Data)

makeLenses ''NodeOptions

parseNodeOptions :: Parser NodeOptions
parseNodeOptions = do
  path <-
    option
      somePreDirOpt
      ( long "anoma-dir"
          <> metavar "ANOMA_DIR"
          <> help "Path to anoma repository"
          <> action "directory"
      )
  pure
    NodeOptions
      { _nodeAnomaPath =
          AppPath
            { _pathIsInput = False,
              _pathPath = path
            },
        ..
      }
