module Commands.Dev.Geb.Check where

import Commands.Base
import Commands.Dev.Geb.Infer.Options
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Error

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
  case Geb.runParser' f content of
    Right tyMorph@(Geb.TypedMorphism {}) -> do
      case run . runError @CheckingError $ (Geb.check' tyMorph) of
        Left err -> exitJuvixError (JuvixError err)
        Right _ -> renderStdOut ("Well done! It typechecks" :: Text)
    Left err -> exitJuvixError (JuvixError err)
