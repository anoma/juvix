module Commands.Dev.InstanceTermination.Calls where

import Commands.Base
import Commands.Dev.InstanceTermination.Calls.Options
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew.Options
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context

runCommand :: (Members AppEffects r) => CallsOptions -> Sem r ()
runCommand localOpts@CallsOptions {..} = do
  globalOpts <- askGlobalOptions
  let tco =
        TypeCheckingOptions
          { _typeCheckingMode = TypeCheckingBuildCallMap
          }
  PipelineResult {..} <-
    runReader tco $
      runPipelineTermination (Just _callsInputFile) upToInternalTypedOptions
  let res :: InstanceCallMaps = snd _pipelineResult ^. resultInstanceCallMaps
  forM_ (res ^. instanceCallMaps) $
    \(block :: InstanceCallMap) -> do
      renderStdOut (ppOut (globalOpts, localOpts) (block ^. instanceCallMap))
      newline
