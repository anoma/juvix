module Commands.Dev.ImportTree.Print where

import Commands.Base
import Commands.Dev.ImportTree.Print.Options
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Pipeline.Loader.PathResolver

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => PrintOptions -> Sem r ()
runCommand PrintOptions {..} = runReader opts . runPipelineSetup $ do
  tree <- ask
  renderStdOut (ppOutDefaultNoComments tree)
  when _printStats $ do
    let stats = mkImportTreeStats tree
    renderStdOut (ppOutDefaultNoComments stats)
  where
    opts =
      defaultPipelineOptions
        { _pipelineImportStrategy = _printScanStrategy
        }
