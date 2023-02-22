module Commands.Dev.Geb.Infer where

import Commands.Base
import Commands.Dev.Geb.Infer.Options
import Juvix.Compiler.Backend.Geb qualified as Geb

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
    Right (Geb.ExpressionMorphism gebTerm) ->
      case Geb.inferObject' gebTerm of
        Left err -> exitJuvixError (JuvixError err)
        Right obj -> renderStdOut (Geb.ppOut opts obj)
    Right (Geb.ExpressionObject _) ->
      exitJuvixError (error @JuvixError "No inference for objects")
    Left err -> exitJuvixError (JuvixError err)
