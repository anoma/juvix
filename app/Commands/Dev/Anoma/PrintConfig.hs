module Commands.Dev.Anoma.PrintConfig where

import Commands.Base
import Commands.Dev.Anoma.Client
import Commands.Dev.Anoma.PrintConfig.Options
import Data.Yaml qualified as Yaml

runCommand :: (Members AppEffects r) => PrintConfigOptions -> Sem r ()
runCommand _ = runAppError @SimpleError $ do
  cfg <- (^. clientConfigHost) <$> getClientConfig
  renderStdOutRaw (Yaml.encode cfg)
