module Commands.Dev.ImportTree.ScanFile where

import Commands.Base
import Commands.Dev.ImportTree.ScanFile.Options
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.ImportScanner.FlatParse

runCommand :: (Members '[App, EmbedIO] r) => ScanFileOptions -> Sem r ()
runCommand ScanFileOptions {..} = runFilesIO $ do
  imports <- fromAppPathFile _scanFileFile >>= scanFileImports
  forM_ imports $ \impor -> do
    renderStdOut (prettyText impor)
    newline
