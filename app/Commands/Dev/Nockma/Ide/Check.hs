module Commands.Dev.Nockma.Ide.Check where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Ide.Check.Options
import Juvix.Compiler.Nockma.Highlight
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members AppEffects r) => NockmaCheckOptions -> Sem r ()
runCommand opts = silenceProgressLog . runPipelineOptions $ do
  afile <- fromAppPathFile (opts ^. nockmaCheckFile)
  void
    . runAppError @JuvixError
    . ignoreHighlightBuilder
    $ Nockma.parseTermFile afile
  renderStdOutLn ("Ok" :: Text)
