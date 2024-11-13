module Commands.Dev.Anoma
  ( module Commands.Dev.Anoma,
    module Commands.Dev.Anoma.Options,
  )
where

import Commands.Base
import Commands.Dev.Anoma.Node qualified as Node
import Commands.Dev.Anoma.Options

runCommand :: (Members AppEffects r) => AnomaCommand -> Sem r ()
runCommand = \case
  AnomaCommandNode opts -> Node.runCommand opts
