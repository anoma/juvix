module Commands.Dev.Internal.Arity where

import Commands.Base
import Commands.Dev.Internal.Arity.Options
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as InternalArity

runCommand :: (Members '[Embed IO, App] r) => InternalArityOptions -> Sem r ()
runCommand opts = do
  globalOpts <- askGlobalOptions
  micro <- head . (^. InternalArity.resultModules) <$> runPipelineTermination (opts ^. internalArityInputFile) upToInternalArity
  renderStdOut (Internal.ppOut globalOpts micro)
