module Commands.Dev.Internal.Pretty where

import Commands.Base
import Commands.Dev.Internal.Pretty.Options
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker

runCommand :: (Members '[Embed IO, App] r) => InternalPrettyOptions -> Sem r ()
runCommand opts = do
  globalOpts <- askGlobalOptions
  intern <-
    head . (^. Internal.resultModules)
      <$> ( runPipeline (opts ^. internalPrettyInputFile)
              . evalTermination iniTerminationState
              $ upToInternal
          )
  renderStdOut (Internal.ppOut globalOpts intern)
