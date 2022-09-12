module Commands.Dev.Internal.Pretty where

import Commands.Base
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromAbstract qualified as Internal

runCommand :: Members '[Embed IO, App] r => EntryPoint -> Sem r ()
runCommand entryPoint = do
  globalOpts <- askGlobalOptions
  micro <- head . (^. Internal.resultModules) <$> runPipeline (upToInternal entryPoint)
  renderStdOut (Internal.ppOut globalOpts micro)
