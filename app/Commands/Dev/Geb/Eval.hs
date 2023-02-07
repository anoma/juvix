module Commands.Dev.Geb.Eval where

import Commands.Base
import Commands.Dev.Geb.Eval.Options
import Juvix.Compiler.Backend.Geb.Evaluator qualified as Geb
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource qualified as Geb

runCommand ::
  forall r a.
  ( Members '[App, Embed IO] r,
    CanonicalProjection a Geb.EvaluatorOptions,
    CanonicalProjection a GebEvalOptions
  ) =>
  a ->
  Sem r ()
runCommand opts = do
  let b :: SomeBase File
      b = project opts ^. gebEvalOptionsInputFile . pathPath
  f :: Path Abs File <- someBaseToAbs' b
  content :: Text <- embed (readFile (toFilePath f))
  case Geb.runParser f content of
    Left err -> exitJuvixError (JuvixError err)
    Right gebTerm -> evalAndPrint opts gebTerm

evalAndPrint ::
  forall r a.
  ( Members '[App, Embed IO] r,
    CanonicalProjection a Geb.EvaluatorOptions
  ) =>
  a ->
  Geb.Expression ->
  Sem r ()
evalAndPrint opts = \case
  Geb.ExpressionMorphism morphism -> do
    let opts' :: Geb.EvaluatorOptions = project opts
    let env :: Geb.Env =
          Geb.Env
            { _envEvaluatorOptions = opts',
              _envContext = Geb.emptyContext
            }
    nf <- runM . runError . runReader env $ Geb.nf morphism
    case nf of
      Left err -> exitJuvixError err
      Right m -> renderStdOut (Geb.ppOut opts' m)
  Geb.ExpressionObject _ -> error gebObjNoEvalMsg

gebObjNoEvalMsg :: Text
gebObjNoEvalMsg = "Geb objects cannot be evaluated, only morphisms."

data RunEvalArgs = RunEvalArgs
  { -- | The input file
    _runEvalArgsInputFile :: Path Abs File,
    -- | The content of the input file
    _runEvalArgsContent :: Text,
    -- | The options
    _runEvalArgsEvaluatorOptions :: Geb.EvaluatorOptions
  }

makeLenses ''RunEvalArgs

runEval :: RunEvalArgs -> Either JuvixError Geb.Expression
runEval RunEvalArgs {..} =
  case Geb.runParser _runEvalArgsInputFile _runEvalArgsContent of
    Right (Geb.ExpressionMorphism morphism) -> do
      let env :: Geb.Env =
            Geb.Env
              { _envEvaluatorOptions = _runEvalArgsEvaluatorOptions,
                _envContext = Geb.emptyContext
              }
      Geb.ExpressionMorphism <$> Geb.nf' morphism env
    Right _ -> Left (error @JuvixError gebObjNoEvalMsg)
    Left err -> Left (JuvixError err)
