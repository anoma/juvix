module Commands.Dev.Highlight where

import Commands.Base
import Commands.Dev.Highlight.Options
import Juvix.Compiler.Concrete.Data.Highlight qualified as Highlight
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser

runCommand :: Members '[Embed IO, App] r => HighlightOptions -> Sem r ()
runCommand HighlightOptions {..} = do
  res <- runPipelineEither _highlightInputFile upToScoping
  case res of
    Left err -> do
      genOpts <- askGenericOptions
      say (Highlight.goError (run $ runReader genOpts $ errorIntervals err))
    Right r -> do
      let tbl = r ^. _2 . Scoper.resultParserTable
          items = tbl ^. Parser.infoParsedItems
          names = r ^. _2 . (Scoper.resultScoperTable . Scoper.infoNames)
          inputFile = _highlightInputFile ^. pathPath
          hinput =
            Highlight.filterInput
              inputFile
              Highlight.HighlightInput
                { _highlightNames = names,
                  _highlightParsed = items
                }
      raw (Highlight.go _highlightBackend hinput)
