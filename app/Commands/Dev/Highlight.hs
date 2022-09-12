module Commands.Dev.Highlight where

import Commands.Base
import Commands.Dev.Highlight.Options
import Juvix.Compiler.Concrete.Data.Highlight qualified as Highlight
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser

runCommand :: Members '[Embed IO, App] r => EntryPoint -> HighlightOptions -> Sem r ()
runCommand entryPoint HighlightOptions {..} = do
  res <- runPipelineEither (upToScoping entryPoint)
  case res of
    Left err -> say (Highlight.goError (run $ runReader (entryPoint ^. entryPointGenericOptions) $ errorIntervals err))
    Right r -> do
      let tbl = r ^. Scoper.resultParserTable
          items = tbl ^. Parser.infoParsedItems
          names = r ^. (Scoper.resultScoperTable . Scoper.infoNames)
          inputFile = entryPoint ^. mainModulePath
          hinput =
            Highlight.filterInput
              inputFile
              Highlight.HighlightInput
                { _highlightNames = names,
                  _highlightParsed = items
                }
      raw (Highlight.go _highlightBackend hinput)
