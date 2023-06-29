module Commands.Dev.Internal.Reachability where

import Commands.Base
import Commands.Dev.Internal.Reachability.Options
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete qualified as Internal

runCommand :: (Members '[Embed IO, App] r) => InternalReachabilityOptions -> Sem r ()
runCommand opts = do
  globalOpts <- askGlobalOptions
  depInfo <- (^. Internal.resultDepInfo) <$> runPipeline (opts ^. internalReachabilityInputFile) upToInternal
  renderStdOut (Internal.ppOut globalOpts depInfo)
