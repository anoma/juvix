module Commands.Dev.Highlight where

import Commands.Base
import Commands.Dev.Highlight.Options
import Juvix.Compiler.Concrete.Data.Highlight qualified as Highlight

runCommand :: (Members AppEffects r) => HighlightOptions -> Sem r ()
runCommand HighlightOptions {..} = silenceProgressLog . runPipelineOptions $ do
  entry <- getEntryPoint (Just _highlightInputFile)
  inputFile <- fromAppPathFile _highlightInputFile
  hinput <-
    Highlight.filterInput
      inputFile
      <$> runPipelineHighlight entry upToInternalTyped
  renderStdOutRaw (Highlight.highlight _highlightBackend hinput)
  newline
