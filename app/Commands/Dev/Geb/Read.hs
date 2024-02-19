module Commands.Dev.Geb.Read where

import Commands.Base
import Commands.Dev.Geb.Read.Options
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource qualified as Geb

runCommand ::
  forall r.
  (Member App r, Member EmbedIO r) =>
  GebReadOptions ->
  Sem r ()
runCommand opts = do
  let b :: AppPath File
      b = opts ^. gebReadOptionsInputFile
  f :: Path Abs File <- fromAppPathFile b
  content :: Text <- readFile f
  case Geb.runParser f content of
    Left err -> exitJuvixError (JuvixError err)
    Right gebTerm -> do
      renderStdOut (Geb.ppOut opts gebTerm)
      putStrLn ""
