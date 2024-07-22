module Commands.Dev.ImportTree where

import Commands.Base
import Commands.Dev.ImportTree.Options
import Commands.Dev.ImportTree.Print qualified as Print
import Commands.Dev.ImportTree.ScanFile qualified as ScanFile

runCommand :: (Members AppEffects r) => ImportTreeCommand -> Sem r ()
runCommand = \case
  Print opts -> Print.runCommand opts
  ScanFile opts -> ScanFile.runCommand opts
