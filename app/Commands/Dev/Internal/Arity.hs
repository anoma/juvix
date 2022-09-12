module Commands.Dev.Internal.Arity where

import Commands.Base
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as InternalArity

runCommand :: Members '[Embed IO, App] r => EntryPoint -> Sem r ()
runCommand entryPoint = do
  globalOpts <- askGlobalOptions
  micro <- head . (^. InternalArity.resultModules) <$> runPipeline (upToInternalArity entryPoint)
  renderStdOut (Internal.ppOut globalOpts micro)
