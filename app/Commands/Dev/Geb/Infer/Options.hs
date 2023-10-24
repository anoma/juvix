module Commands.Dev.Geb.Infer.Options where

import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb

newtype GebInferOptions = GebInferOptions
  { _gebInferOptionsInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''GebInferOptions

instance CanonicalProjection GebInferOptions Geb.Options where
  project _ = Geb.defaultOptions

parseGebInferOptions :: Parser GebInferOptions
parseGebInferOptions = do
  _gebInferOptionsInputFile <-
    (parseInputFiles (NonEmpty.fromList [FileExtJuvixGeb, FileExtJuvix]))
  pure GebInferOptions {..}
