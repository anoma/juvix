module Commands.Dev.PlainMarkdown.Format where

import Commands.Base
import Commands.Dev.PlainMarkdown.Format.Options
import Markdown.FromSource
import Markdown.Print

runCommand ::
  forall r.
  (Members AppEffects r) =>
  FormatOptions ->
  Sem r ()
runCommand opts = do
  afile <- fromAppPathFile (opts ^. formatFile)
  mdBlock <- runAppError @SimpleError (fromFile afile)
  print mdBlock
  renderStdOutLn (ppOut mdBlock)
