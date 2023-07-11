module Commands.Dev.Geb.Check where

import Commands.Base
import Commands.Dev.Geb.Infer.Options
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty

runCommand ::
  forall r.
  (Member App r, Member (Embed IO) r) =>
  GebInferOptions ->
  Sem r ()
runCommand opts = do
  let b :: AppPath File
      b = opts ^. gebInferOptionsInputFile
  f :: Path Abs File <- fromAppPathFile b
  content :: Text <- embed (readFile (toFilePath f))
  case Geb.runParser f content of
    Right (Geb.ExpressionMorphism morph) -> do
      case Geb.inferObject' morph of
        Left err -> exitJuvixError (JuvixError err)
        Right ty -> do
          renderStdOut (ppOutDefault ty)
          embed (putStrLn "")
    Right _ -> exitJuvixError (error @JuvixError "Not a morphism")
    Left err -> exitJuvixError (JuvixError err)
