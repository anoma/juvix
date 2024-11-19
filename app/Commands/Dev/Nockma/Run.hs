module Commands.Dev.Nockma.Run where

import Commands.Base
import Commands.Dev.Nockma.Run.BuiltinEvaluator as BuiltinEvaluator
import Commands.Dev.Nockma.Run.EphemeralClient as EphemeralClient
import Commands.Dev.Nockma.Run.Options
import Commands.Dev.Nockma.Run.WithClient as WithClient

runCommand :: forall r. (Members AppEffects r) => NockmaRunCommand -> Sem r ()
runCommand = \case
  NockmaRunBuiltinEvaluator opts -> BuiltinEvaluator.runCommand opts
  NockmaRunEphemeralClient opts -> EphemeralClient.runCommand opts
  NockmaRunWithClient opts -> WithClient.runCommand opts
