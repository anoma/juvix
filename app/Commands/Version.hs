module Commands.Version where

import Commands.Base
import Commands.Version.Options
import Juvix.Extra.Version

runCommand :: (Members AppEffects r) => VersionCommand -> Sem r ()
runCommand = \case
  VersionHuman -> runDisplayVersion
  VersionNumeric -> runDisplayNumericVersion
  VersionCommit -> runDisplayFullVersion
