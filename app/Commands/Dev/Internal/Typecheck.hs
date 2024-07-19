module Commands.Dev.Internal.Typecheck where

import Commands.Base
import Commands.Dev.Internal.Typecheck.Options
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped

runCommand :: (Members AppEffects r) => InternalTypeOptions -> Sem r ()
runCommand localOpts = do
  globalOpts <- askGlobalOptions
  res <- runPipelineNoOptions (localOpts ^. internalTypeInputFile) upToInternalTyped
  logInfo "Well done! It type checks"
  when (localOpts ^. internalTypePrint) $ do
    let checkedModule = res ^. InternalTyped.resultModule
    renderStdOut (Internal.ppOut globalOpts checkedModule)
