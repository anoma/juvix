module Commands.Dev.Latex.Export
  ( module Commands.Dev.Latex.Export,
    module Commands.Dev.Latex.Export.Options,
  )
where

import Commands.Base
import Commands.Dev.Latex.Export.Options
import Juvix.Compiler.Backend.Latex.Translation.FromScoped.Source
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper

runCommand :: (Members AppEffects r) => ExportOptions -> Sem r ()
runCommand ExportOptions {..} = do
  res :: Scoper.ScoperResult <- silenceProgressLog (runPipelineNoOptions (Just _exportInputFile) upToScopingEntry)
  let m :: Module 'Scoped 'ModuleTop = res ^. Scoper.resultModule
      c :: Maybe Comments = guard (not _exportNoComments) $> Scoper.getScoperResultComments res
  genSourceLatex c m
