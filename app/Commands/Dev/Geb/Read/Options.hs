module Commands.Dev.Geb.Read.Options where

import CommonOptions
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb

newtype GebReadOptions = GebReadOptions
  { _gebReadOptionsInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''GebReadOptions

instance CanonicalProjection GebReadOptions Geb.Options where
  project _ = Geb.defaultOptions

parseGebReadOptions :: Parser GebReadOptions
parseGebReadOptions = do
  _gebReadOptionsInputFile <- parseInputJuvixGebFile
  pure GebReadOptions {..}
