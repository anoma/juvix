module Commands.Dev.Anoma.Node where

import Anoma.Effect
import Commands.Base
import Commands.Dev.Anoma.Node.Options

runCommand :: forall r. (Members AppEffects r) => NodeOptions -> Sem r ()
runCommand opts = runConcurrent . runProcess $ do
  anomaDir :: AnomaPath <- AnomaPath <$> fromAppPathDir (opts ^. nodeAnomaPath)
  runAnoma anomaDir $ do
    void noHalt
