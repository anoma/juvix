module Commands.Dev.ImportTree.ScanFile (runCommand) where

import Commands.Base
import Commands.Dev.ImportTree.ScanFile.Options
import Data.Yaml qualified as Yaml
import Juvix.Compiler.Concrete.Print
import Juvix.Compiler.Concrete.Translation.ImportScanner

runCommand :: (Members AppEffects r) => ScanFileOptions -> Sem r ()
runCommand opts@ScanFileOptions {..} =
  runFilesIO
    . runAppError @ParserError
    . runReader _scanFileStrategy
    $ do
      p <- fromAppPathFile _scanFileFile
      scanRes <- scanFileImports p
      printRes opts scanRes
      when _scanFileCheck (check p scanRes)

printRes :: (Members (AppEffects) r) => ScanFileOptions -> ScanResult -> Sem r ()
printRes ScanFileOptions {..} scanRes = do
  forM_ (scanRes ^. scanResultImports) $ \impor -> do
    opts <- askGenericOptions
    renderStdOut (ppOutNoComments opts impor)
    when _scanFilePrintLoc $ do
      renderStdOut @Text " "
      renderStdOut (ppOutNoComments opts (getLoc impor))
    newline

check :: (Members (Reader ImportScanStrategy ': AppEffects) r) => Path Abs File -> ScanResult -> Sem r ()
check file reference = runAppError @ParserError $ do
  refStrat :: ImportScanStrategy <- ask
  forM_ allElements $ \strat -> when (refStrat /= strat) . local (const strat) $ do
    res <- scanFileImports file
    let yamlFile :: ImportScanStrategy -> Path Abs File
        yamlFile s = replaceExtensions' ["." <> show s, ".yaml"] file
    let err :: AnsiText
        err =
          mkAnsiText @Text
            $ prettyText refStrat
            <> " and "
            <> prettyText strat
            <> " don't match"
            <> "\n"
            <> prettyText refStrat
            <> " written to:\n"
            <> toFilePath (yamlFile refStrat)
            <> "\n"
            <> prettyText strat
            <> " written to:\n"
            <> toFilePath (yamlFile strat)
            <> "\n"
    unless (res == reference) $ do
      liftIO (Yaml.encodeFile (toFilePath (yamlFile refStrat)) reference)
      liftIO (Yaml.encodeFile (toFilePath (yamlFile strat)) res)
      logErrorWithTag err
