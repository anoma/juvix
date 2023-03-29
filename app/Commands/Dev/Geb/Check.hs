module Commands.Dev.Geb.Check where

import Commands.Base
import Commands.Dev.Geb.Infer.Options
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Error

runCommand ::
  forall r.
  (Member App r, Member (Embed IO) r) =>
  GebInferOptions ->
  Sem r ()
runCommand opts = do
  let b :: SomeBase File
      b = opts ^. gebInferOptionsInputFile . pathPath
  f :: Path Abs File <- someBaseToAbs' b
  content :: Text <- embed (readFile (toFilePath f))
  case Geb.runParser f content of
    Right (Geb.ExpressionTypedMorphism tyMorph) -> do
      case run . runError @CheckingError $ (Geb.check' tyMorph) of
        Left err -> exitJuvixError (JuvixError err)
        Right _ -> renderStdOut ("Well done! It typechecks\n" :: Text)
    Right _ -> exitJuvixError (error @JuvixError "Not a typed morphism")
    Left err -> exitJuvixError (JuvixError err)
