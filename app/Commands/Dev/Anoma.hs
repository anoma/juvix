module Commands.Dev.Anoma
  ( module Commands.Dev.Anoma,
    module Commands.Dev.Anoma.Options,
  )
where

import Commands.Base
import Commands.Dev.Anoma.Options
import Commands.Dev.Anoma.Start qualified as Start

runCommand :: (Members AppEffects r) => AnomaCommand -> Sem r ()
runCommand = \case
  AnomaCommandStart opts -> Start.runCommand opts
