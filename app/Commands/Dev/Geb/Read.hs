module Commands.Dev.Geb.Read where

import Commands.Base
import Commands.Dev.Geb.Read.Options
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource qualified as Geb

runCommand ::
  forall r.
  (Member App r, Member (Embed IO) r) =>
  GebReadOptions ->
  Sem r ()
runCommand opts = do
  let b :: SomeBase File
      b = opts ^. gebReadOptionsInputFile . pathPath
  f :: Path Abs File <- someBaseToAbs' b
  content :: Text <- embed (readFile (toFilePath f))
  case Geb.runParser f content of
    Left err -> exitJuvixError (JuvixError err)
    Right gebTerm -> do
      renderStdOut (Geb.ppOut opts gebTerm)
      embed (putStrLn "")
