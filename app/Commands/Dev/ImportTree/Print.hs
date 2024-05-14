module Commands.Dev.ImportTree.Print where

import Commands.Base
import Commands.Dev.ImportTree.Print.Options
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Setup
import Juvix.Parser.Error

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => PrintOptions -> Sem r ()
runCommand PrintOptions {..} = do
  inputFile <- mapM fromAppPathFile _printInputFile
  runPipelineSetup $
    do
      entrySetup defaultDependenciesConfig
      tree <-
        runReader _printScanStrategy
          . mapError (JuvixError @ParserError)
          . mapError (JuvixError @ScoperError)
          $ mkImportTree inputFile
      renderStdOut (ppOutDefaultNoComments tree)
      when _printStats $ do
        let stats = mkImportTreeStats tree
        renderStdOut (ppOutDefaultNoComments stats)
