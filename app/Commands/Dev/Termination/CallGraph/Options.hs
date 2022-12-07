module Commands.Dev.Termination.CallGraph.Options where

import CommonOptions
import Data.Text qualified as Text

data CallGraphOptions = CallGraphOptions
  { _graphFunctionNameFilter :: Maybe (NonEmpty Text),
    _graphInputFile :: AppPath File
  }

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
  _graphInputFile <- parseInputJuvixFile
  pure CallGraphOptions {..}
