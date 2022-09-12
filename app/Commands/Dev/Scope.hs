module Commands.Dev.Scope where

import Commands.Base
import Commands.Dev.Scope.Options
import Juvix.Compiler.Concrete.Pretty qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper

runCommand :: Members '[Embed IO, App] r => ScopeOptions -> Sem r ()
runCommand opts = do
  globalOpts <- askGlobalOptions
  l <-
    (^. Scoper.resultModules)
      <$> runPipeline (opts ^. scopeInputFile) upToScoping
  forM_ l $ \s -> do
    renderStdOut (Scoper.ppOut (globalOpts, opts) s)
