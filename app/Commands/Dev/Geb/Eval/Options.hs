module Commands.Dev.Geb.Eval.Options where

import CommonOptions
import Juvix.Compiler.Backend.Geb.Evaluator qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb

data GebEvalOptions = GebEvalOptions
  { _gebEvalOptionsInputFile :: AppPath File,
    _gebEvalOptionsOutputMorphism :: Bool
  }
  deriving stock (Data)

makeLenses ''GebEvalOptions

instance CanonicalProjection GebEvalOptions Geb.EvaluatorOptions where
  project x =
    Geb.EvaluatorOptions
      { _evaluatorOptionsOutputMorphism = (x ^. gebEvalOptionsOutputMorphism)
      }

instance CanonicalProjection GebEvalOptions Geb.Options where
  project _ = Geb.defaultOptions

parseGebEvalOptions :: Parser GebEvalOptions
parseGebEvalOptions = do
  _gebEvalOptionsInputFile <- parseInputJuvixGebFile
  _gebEvalOptionsOutputMorphism <- optOutputMorphism
  pure GebEvalOptions {..}

optOutputMorphism :: Parser Bool
optOutputMorphism =
  switch
    ( long "output-morphism"
        <> short 'm'
        <> showDefault
        <> help "Output a Geb morphism back from a Geb value"
    )
