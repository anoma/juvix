module Commands.Dev.Geb.Read.Options where

import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty
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
  _gebReadOptionsInputFile <-
    parseInputFiles (NonEmpty.fromList [FileExtJuvixGeb, FileExtJuvix])
  pure GebReadOptions {..}
