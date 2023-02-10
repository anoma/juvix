module Commands.Dev.Geb.Eval where

import Commands.Base
import Commands.Dev.Geb.Eval.Options
import Juvix.Compiler.Backend.Geb.Data.Context as EvalContext
import Juvix.Compiler.Backend.Geb.Evaluator qualified as Geb
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty.Values qualified as GebValue
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
              _envContext = EvalContext.empty
            }
    case Geb.eval' env morphism of
      Left err -> exitJuvixError err
      Right m -> renderStdOut (GebValue.ppOut opts' m)
  Geb.ExpressionObject _ -> error Geb.objNoEvalMsg
