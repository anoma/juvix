module Commands.Dev.Highlight where

import Commands.Base
import Commands.Dev.Highlight.Options
import Juvix.Compiler.Concrete.Data.Highlight qualified as Highlight
import Juvix.Compiler.Pipeline.Run

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => HighlightOptions -> Sem r ()
runCommand HighlightOptions {..} = runReader defaultPipelineOptions $ do
  entry <- getEntryPoint (Just _highlightInputFile)
  inputFile <- fromAppPathFile _highlightInputFile
  hinput <-
    Highlight.filterInput
      inputFile
      <$> runPipelineHighlight entry upToInternalTyped
  sayRaw (Highlight.highlight _highlightBackend hinput)
