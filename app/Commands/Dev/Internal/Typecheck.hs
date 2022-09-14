module Commands.Dev.Internal.Typecheck where

import Commands.Base
import Commands.Dev.Internal.Typecheck.Options
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped

runCommand :: Members '[Embed IO, App] r => InternalTypeOptions -> Sem r ()
runCommand localOpts = do
  globalOpts <- askGlobalOptions
  res <- runPipeline (localOpts ^. internalTypeInputFile) upToInternalTyped
  say "Well done! It type checks"
  when (localOpts ^. internalTypePrint) $ do
    let checkedModule = head (res ^. InternalTyped.resultModules)
    renderStdOut (Internal.ppOut globalOpts checkedModule)
