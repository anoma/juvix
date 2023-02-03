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
    let opts' = project opts
    let res = Geb.eval opts' m
    renderStdOut (Geb.ppOut opts' res)
  Geb.ExpressionObject _ -> do
    error gebObjNoEvalMsg

gebObjNoEvalMsg :: Text
gebObjNoEvalMsg =
  "Geb objects cannot be evaluated. \
  \Please try with a morphism instead."

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
    Left err -> Left (JuvixError err)
    Right (Geb.ExpressionMorphism gebMorph) ->
      Right
        (Geb.ExpressionMorphism (Geb.eval _runEvalArgsEvaluatorOptions gebMorph))
    Right (Geb.ExpressionObject _) -> Left (error @JuvixError gebObjNoEvalMsg)
