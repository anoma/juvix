module Commands.Dev.ImportTree where

import Commands.Base
import Commands.Dev.ImportTree.Options
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Setup

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => ImportTreeOptions -> Sem r ()
runCommand ImportTreeOptions =
  runPipelineSetup $ do
    entrySetup defaultDependenciesConfig
    renderStdOut @Text "entry setup done"
    tree <- mapError (JuvixError @ScoperError) mkImportTree
    renderStdOut (ppOutDefaultNoComments tree)
