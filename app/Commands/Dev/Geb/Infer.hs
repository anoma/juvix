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
  let b :: AppPath File
      b = opts ^. gebInferOptionsInputFile
  f :: Path Abs File <- fromAppPathFile b
  content :: Text <- readFile (toFilePath f)
  case Geb.runParser f content of
    Right (Geb.ExpressionMorphism gebTerm) ->
      case Geb.inferObject' gebTerm of
        Left err -> exitJuvixError (JuvixError err)
        Right obj -> renderStdOut (Geb.ppOut opts obj)
    Right (Geb.ExpressionTypedMorphism tyMorph) -> do
      case run . runError @Geb.CheckingError $ Geb.check' tyMorph of
        Left err -> exitJuvixError (JuvixError err)
        Right _ -> do
          renderStdOut $
            Geb.ppOut
              opts
              (tyMorph ^. Geb.typedMorphismObject)
          putStrLn ""
    Right (Geb.ExpressionObject _) ->
      exitJuvixError (error @JuvixError "No inference for objects")
    Left err -> exitJuvixError (JuvixError err)
