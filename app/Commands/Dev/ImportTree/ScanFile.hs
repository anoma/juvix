module Commands.Dev.ImportTree.ScanFile where

import Commands.Base
import Commands.Dev.ImportTree.ScanFile.Options
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.ImportScanner
import Juvix.Compiler.Concrete.Translation.ImportScanner.Base

runCommand :: (Members '[App, EmbedIO] r) => ScanFileOptions -> Sem r ()
runCommand ScanFileOptions {..} =
  runFilesIO
    . runAppError @ParserError
    . runReader _scanFileStrategy
    $ do
      scanRes <- fromAppPathFile _scanFileFile >>= scanFileImports
      renderStdOut (prettyText (scanRes ^. scanResultModule))
      forM_ (scanRes ^. scanResultImports) $ \impor -> do
        renderStdOut (prettyText impor)
        newline
