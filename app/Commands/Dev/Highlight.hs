module Commands.Dev.Highlight where

import Commands.Base
import Commands.Dev.Highlight.Options
import Juvix.Compiler.Concrete.Data.Highlight (HighlightInput (..))
import Juvix.Compiler.Concrete.Data.Highlight qualified as Highlight
import Juvix.Compiler.Concrete.Data.Highlight.Input (highlightErrors, highlightParsed)
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoper
import Juvix.Compiler.Concrete.Data.ScopedName (AName)
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver (PathResolver, evalPathResolverPipe)
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal

runCommand :: Members '[Embed IO, App] r => HighlightOptions -> Sem r ()
runCommand HighlightOptions {..} = do
  res <- fmap snd <$> runPipelineEither _highlightInputFile upToInternalTyped
  inputFile <- fromAppPathFile _highlightInputFile
  case res of
    Left err -> do
      let filterByFile = filter ((== inputFile) . (^. intervalFile))
      genOpts <- askGenericOptions
      sayRaw (Highlight.goErrors _highlightBackend (filterByFile . run . runReader genOpts $ errorIntervals err))
    Right r -> do
      let scoperResult = r ^. Internal.internalTypedResultScoped
          tbl :: Parser.InfoTable = scoperResult ^. Scoper.resultParserResult . Parser.resultTable
          items = tbl ^. Parser.infoParsedItems
          -- names :: [AName] = scoperResult ^. Scoper.resultScoperTable . Scoper.infoNames
          -- docs = scoperResult ^. Scoper.resultScoperTable . Scoper.infoDoc
          types = r ^. Internal.resultIdenTypes
          hinput =
            Highlight.filterInput
              inputFile
              undefined
      -- HighlightInput
      --   { _highlightNames = names,
      --     _highlightParsed = items,
      --     _highlightDoc = docs,
      --     _highlightErrors = [],
      --     _highlightTypes = types
      --   }
      sayRaw (Highlight.highlight _highlightBackend hinput)

-- | Runs the pipeline stepwise and collects relevant highlighting information
goPipeline :: Members '[Embed IO, App, State HighlightInput] r => HighlightOptions -> Sem r ()
goPipeline HighlightOptions {..} = do
  entry <- getEntryPoint _highlightInputFile
  genOpts <- askGenericOptions
  runFilesIO
    . evalTopNameIdGen
    . runReader entry
    . runReader genOpts
    . evalPathResolverPipe
    . ignoreFail
    $ go
  where
    go :: forall r. Members '[Reader GenericOptions, Fail, Reader EntryPoint, Files, NameIdGen, PathResolver, State HighlightInput] r => Sem r ()
    go = do
      r <- runJuvixError upToParsing
      let tbl :: Parser.InfoTable = r ^. Parser.resultTable
          items = tbl ^. Parser.infoParsedItems
      modify' (set highlightParsed items)
      s <- runError @JuvixError (Scoper.fromParsed r)
      undefined
      where
        runJuvixError :: Sem (Error JuvixError ': r) a -> Sem r a
        runJuvixError m = do
          genOpts <- ask @GenericOptions
          r <- runError @JuvixError m
          case r of
            Right res -> return res
            Left err -> do
              let errLocs :: [Interval] = run (runReader genOpts (genericError err)) ^. genericErrorIntervals
              modify' (set highlightErrors errLocs)
              fail
