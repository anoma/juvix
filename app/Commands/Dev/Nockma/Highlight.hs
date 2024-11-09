module Commands.Dev.Nockma.Highlight where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Highlight.Options
import Juvix.Compiler.Nockma.Highlight
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members AppEffects r) => NockmaHighlightOptions -> Sem r ()
runCommand opts = silenceProgressLog . runPipelineOptions $ do
  afile <- fromAppPathFile (opts ^. nockmaHighlightFile)
  hinput <-
    fmap (filterInput afile)
      . runAppError @JuvixError
      . execHighlightBuilder
      $ Nockma.parseTermFile afile
  renderStdOutRaw (highlight hinput)
  newline
