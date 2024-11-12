module Commands.Dev.Nockma.Ide where

import Commands.Base
import Commands.Dev.Nockma.Ide.Check as Check
import Commands.Dev.Nockma.Ide.Highlight as Highlight
import Commands.Dev.Nockma.Ide.Options

runCommand ::
  forall r.
  (Members AppEffects r) =>
  NockmaIdeCommand ->
  Sem r ()
runCommand = \case
  NockmaIdeHighlight opts -> Highlight.runCommand opts
  NockmaIdeCheck opts -> Check.runCommand opts
