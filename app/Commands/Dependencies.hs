module Commands.Dependencies
  ( module Commands.Dependencies,
    module Commands.Dependencies.Options,
  )
where

import Commands.Base
import Commands.Dependencies.Options
import Commands.Dependencies.Update qualified as Update

runCommand :: (Members '[Embed IO, App] r) => DependenciesCommand -> Sem r ()
runCommand = \case
  Update -> Update.runCommand
