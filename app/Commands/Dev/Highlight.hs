module Commands.Dev.Highlight where

import Commands.Base
import Commands.Dev.Highlight.Options
import Juvix.Compiler.Concrete.Data.Highlight qualified as Highlight
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal

runCommand :: Members '[Embed IO, App] r => HighlightOptions -> Sem r ()
runCommand HighlightOptions {..} = do
  res <- fmap snd <$> runPipelineEither _highlightInputFile upToInternalTyped
  inputFile <- someBaseToAbs' (_highlightInputFile ^. pathPath)
  case res of
    Left err -> do
      let filterByFile = filter ((== inputFile) . (^. intervalFile))
      genOpts <- askGenericOptions
      sayRaw (Highlight.goErrors _highlightBackend (filterByFile . run . runReader genOpts $ errorIntervals err))
    Right r -> do
      let scoperResult = r ^. Internal.internalTypedResultScoped
          tbl = scoperResult ^. Scoper.resultParserResult . Parser.resultTable
          items = tbl ^. Parser.infoParsedItems
          names = scoperResult ^. Scoper.resultScoperTable . Scoper.infoNames
          hinput =
            Highlight.filterInput
              inputFile
              Highlight.HighlightInput
                { _highlightNames = names,
                  _highlightParsed = items,
                  _highlightDoc = scoperResult ^. Scoper.resultScoperTable . Scoper.infoDoc,
                  _highlightTypes = r ^. Internal.resultIdenTypes
                }
      sayRaw (Highlight.highlight _highlightBackend hinput)
