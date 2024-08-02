module Commands.Dev.Scope where

import Commands.Base
import Commands.Dev.Scope.Options
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print qualified as Print
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper

runCommand :: (Members AppEffects r) => ScopeOptions -> Sem r ()
runCommand opts = do
  globalOpts <- askGlobalOptions
  res :: Scoper.ScoperResult <- runPipelineNoOptions (opts ^. scopeInputFile) upToScopingEntry
  let m :: Module 'Scoped 'ModuleTop = res ^. Scoper.resultModule
  if
      | opts ^. scopeWithComments ->
          renderStdOut (Print.ppOut (globalOpts, opts) (Scoper.getScoperResultComments res) m)
      | otherwise ->
          renderStdOut (Print.ppOutNoComments (globalOpts, opts) m)
  when (opts ^. scopeListComments) $ do
    newline
    newline
    renderStdOutLn @Text "Comments:"
    renderStdOutLn (prettyText (Scoper.getScoperResultComments res))
