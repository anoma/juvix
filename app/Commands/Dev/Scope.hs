module Commands.Dev.Scope where

import Commands.Base
import Commands.Dev.Scope.Options
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print qualified as Print
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Prelude.Pretty

runCommand :: (Members '[Embed IO, App] r) => ScopeOptions -> Sem r ()
runCommand opts = do
  globalOpts <- askGlobalOptions
  res :: Scoper.ScoperResult <- runPipeline (opts ^. scopeInputFile) upToScoping
  let modules :: NonEmpty (Module 'Scoped 'ModuleTop) = res ^. Scoper.resultModules
  forM_ modules $ \s ->
    if
        | opts ^. scopeWithComments ->
            renderStdOut (Print.ppOut (globalOpts, opts) (res ^. Scoper.comments) s)
        | otherwise ->
            renderStdOut (Print.ppOutNoComments (globalOpts, opts) s)
  when (opts ^. scopeListComments) $ do
    newline
    newline
    say "Comments:"
    say (prettyText (res ^. Scoper.comments))
