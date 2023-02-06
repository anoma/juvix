module Commands.Dev.Geb.Eval where

import Commands.Base
import Commands.Dev.Geb.Eval.Options
import Juvix.Compiler.Backend.Geb.Evaluator qualified as Geb
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource qualified as Geb

runCommand ::
  forall r a.
  ( (Member App r, Member (Embed IO) r),
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
  ( (Member App r, Member (Embed IO) r),
    CanonicalProjection a Geb.EvaluatorOptions
  ) =>
  a ->
  Geb.Expression ->
  Sem r ()
evalAndPrint opts = \case
  Geb.ExpressionMorphism m -> do
    let opts' :: Geb.EvaluatorOptions = project opts
        morphism' =
          Geb.eval'
            Geb.EvalArgs
              { _evalOptions = opts',
                _evalTerm = m,
                _evalContext = Geb.emptyContext
              }
    case morphism' of
      Left err -> exitJuvixError (JuvixError err)
      Right morphism -> renderStdOut (Geb.ppOut opts' morphism)
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
    Right (Geb.ExpressionMorphism gebMorph) ->
      Geb.ExpressionMorphism
        <$> ( Geb.eval'
                Geb.EvalArgs
                  { _evalOptions = _runEvalArgsEvaluatorOptions,
                    _evalTerm = gebMorph,
                    _evalContext = Geb.emptyContext
                  }
            )
    Right (Geb.ExpressionObject _) -> Left (error @JuvixError gebObjNoEvalMsg)
    Left err -> Left (JuvixError err)
