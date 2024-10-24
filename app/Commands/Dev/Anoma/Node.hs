module Commands.Dev.Anoma.Node where

import Commands.Base
import Commands.Dev.Anoma.Node.Options

runCommand :: forall r. (Members AppEffects r) => NodeOptions -> Sem r ()
runCommand opts = do
  return ()
