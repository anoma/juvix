module Commands.Dev.ImportTree.Print where

import Commands.Base
import Commands.Dev.ImportTree.Print.Options
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => PrintOptions -> Sem r ()
runCommand PrintOptions {..} = runReader opts $ do
  inputFile <- mapM fromAppPathFile _printInputFile
  runPipelineSetup $
    do
      tree <- ask
      tree2 <- mkImportTree inputFile
      renderStdOut (ppOutDefaultNoComments tree)
      when _printStats $ do
        let stats = mkImportTreeStats tree
        renderStdOut (ppOutDefaultNoComments stats)
      renderStdOut (ppOutDefaultNoComments tree2)
      when _printStats $ do
        let stats = mkImportTreeStats tree2
        renderStdOut (ppOutDefaultNoComments stats)
  where
    opts =
      defaultPipelineOptions
        { _pipelineImportStrategy = _printScanStrategy
        }
