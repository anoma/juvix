module Commands.Dependencies.Options where

import CommonOptions

data DependenciesCommand
  = Update
  deriving stock (Data)

parseDependenciesCommand :: Parser DependenciesCommand
parseDependenciesCommand = hsubparser commandUpdate

commandUpdate :: Mod CommandFields DependenciesCommand
commandUpdate =
  command "update"
    $ info
      (pure Update)
      (progDesc "Fetch package dependencies and update the lock file")
