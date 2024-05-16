module Commands.Dev.ImportTree.Print where

import Commands.Base
import Commands.Dev.ImportTree.Print.Options
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Parser.Error

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => PrintOptions -> Sem r ()
runCommand PrintOptions {..} = runReader opts $ do
  inputFile <- mapM fromAppPathFile _printInputFile
  runPipelineSetup $
    do
      tree <-
        mapError (JuvixError @ParserError)
          . mapError (JuvixError @ScoperError)
          $ mkImportTree inputFile
      renderStdOut (ppOutDefaultNoComments tree)
      when _printStats $ do
        let stats = mkImportTreeStats tree
        renderStdOut (ppOutDefaultNoComments stats)
  where
    opts =
      defaultPipelineOptions
        { _pipelineImportStrategy = _printScanStrategy
        }
