module Commands.Dev.PlainMarkdown.Format where

import Commands.Base
import Commands.Dev.PlainMarkdown.Format.Options
import Markdown.FromSource qualified as Markdown
import Markdown.Print

runCommand ::
  forall r.
  (Members AppEffects r) =>
  FormatOptions ->
  Sem r ()
runCommand opts = do
  afile <- fromAppPathFile (opts ^. formatFile)
  mdBlock <- runAppError @SimpleError (Markdown.parseFile afile)
  renderStdOutLn (ppOut mdBlock)
