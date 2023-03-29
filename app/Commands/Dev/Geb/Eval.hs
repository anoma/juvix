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
    Right gebTerm -> do
      evalAndPrint opts gebTerm
      embed (putStrLn "")

evalAndPrint ::
  forall r a.
  ( Members '[App, Embed IO] r,
    CanonicalProjection a Geb.EvaluatorOptions
  ) =>
  a ->
  Geb.Expression ->
  Sem r ()
evalAndPrint opts = \case
  Geb.ExpressionObject _ -> error Geb.objNoEvalMsg
  Geb.ExpressionMorphism morphism -> do
    let opts' :: Geb.EvaluatorOptions = project opts
    let env :: Geb.Env =
          Geb.Env
            { _envEvaluatorOptions = opts',
              _envContext = mempty
            }
    if
        | opts' ^. Geb.evaluatorOptionsOutputMorphism ->
            case Geb.evalAndOutputMorphism' env morphism of
              Left err -> exitJuvixError err
              Right m -> renderStdOut (Geb.ppOut opts' m)
        | otherwise ->
            case Geb.eval' env morphism of
              Left err -> exitJuvixError err
              Right m -> renderStdOut (Geb.ppOut opts' m)
  Geb.ExpressionTypedMorphism tyMorph ->
    evalAndPrint
      opts
      (Geb.ExpressionMorphism (tyMorph ^. Geb.typedMorphism))
