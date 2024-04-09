module Commands.Dev.Termination.CallGraph.Options where

import CommonOptions
import Data.Text qualified as Text

data CallGraphOptions = CallGraphOptions
  { _graphFunctionNameFilter :: Maybe (NonEmpty Text),
    _graphInputFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''CallGraphOptions

parseCallGraph :: Parser CallGraphOptions
parseCallGraph = do
  _graphFunctionNameFilter <-
    fmap msum . optional $
      nonEmpty . Text.words
        <$> option
          str
          ( long "function"
              <> short 'f'
              <> help "Only shows the specified function"
          )
  _graphInputFile <- optional (parseInputFile FileExtJuvix)
  pure CallGraphOptions {..}
