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
      . runJuvixErrorHighlight
      . execHighlightBuilder
      $ Nockma.parseTermFile afile
  renderStdOutRaw (highlight hinput)
  newline

runJuvixErrorHighlight :: forall r. Sem (Error JuvixError ': r) HighlightInput -> Sem r HighlightInput
runJuvixErrorHighlight m = do
  res <- runError m
  return $ case res of
    Right r -> r
    Left err ->
      emptyHighlightInput
        { _highlightErrors = [getLoc err]
        }
