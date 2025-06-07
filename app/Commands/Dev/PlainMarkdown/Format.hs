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
  txt <- readFile' afile
  mdBlock <- runAppError @SimpleError (Markdown.parseText afile txt)
  let out = ppOut mdBlock <> ansiTextNewline
  renderStdOut out
  when (opts ^. formatCheck && txt /= toPlainText out) exitFailure
