module Commands.Dev.Latex
  ( module Commands.Dev.Latex,
    module Commands.Dev.Latex.Options,
  )
where

import Commands.Base
import Commands.Dev.Latex.Options
import Juvix.Compiler.Backend.Latex.Translation.FromScoped.Source
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper

runCommand :: (Members AppEffects r) => LatexOptions -> Sem r ()
runCommand LatexOptions {..} = do
  res :: Scoper.ScoperResult <- silenceProgressLog (runPipelineNoOptions (Just _latexInputFile) upToScopingEntry)
  let m :: Module 'Scoped 'ModuleTop = res ^. Scoper.resultModule
      c :: Maybe Comments = guard (not _latexNoComments) $> Scoper.getScoperResultComments res
  genSourceLatex c m
