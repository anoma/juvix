module Commands.Dev.Termination.CallGraph.Options where

import Data.Text qualified as Text
import Juvix.Prelude
import Options.Applicative

newtype CallGraphOptions = CallGraphOptions
  { _graphFunctionNameFilter :: Maybe (NonEmpty Text)
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
  pure CallGraphOptions {..}
