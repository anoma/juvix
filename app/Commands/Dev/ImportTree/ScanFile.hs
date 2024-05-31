module Commands.Dev.ImportTree.ScanFile where

import Commands.Base
import Commands.Dev.ImportTree.ScanFile.Options
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Concrete.Translation.ImportScanner
import Juvix.Compiler.Concrete.Translation.ImportScanner.Base

runCommand :: (Members '[App, EmbedIO] r) => ScanFileOptions -> Sem r ()
runCommand ScanFileOptions {..} =
  runFilesIO
    . runAppError @ParserError
    . runReader _scanFileStrategy
    $ do
      scanRes <- fromAppPathFile _scanFileFile >>= scanFileImports
      forM_ (scanRes ^. scanResultImports) $ \impor -> do
        opts <- askGenericOptions
        renderStdOut (ppOutNoComments opts impor)
        newline
