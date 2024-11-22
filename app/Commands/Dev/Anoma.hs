module Commands.Dev.Anoma
  ( module Commands.Dev.Anoma,
    module Commands.Dev.Anoma.Options,
  )
where

import Commands.Base
import Commands.Dev.Anoma.Options
import Commands.Dev.Anoma.Start qualified as Start
import Commands.Dev.Anoma.Status qualified as Status
import Commands.Dev.Anoma.Stop qualified as Stop

runCommand :: (Members AppEffects r) => AnomaCommand -> Sem r ()
runCommand = \case
  AnomaCommandStart opts -> Start.runCommand opts
  AnomaCommandStatus -> Status.runCommand
  AnomaCommandStop -> Stop.runCommand
