module Commands.Dev.Anoma.Intents where

import Anoma.Effect.Base
import Anoma.Effect.Intents.Verify
import Commands.Base
import Commands.Dev.Anoma.Base
import Commands.Dev.Anoma.Intents.Options
import Commands.Dev.Anoma.Intents.Verify.Options
import Juvix.Compiler.Nockma.Pretty hiding (Path)
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members (Anoma ': Error SimpleError ': AppEffects) r) => AnomaIntentsCommand -> Sem r ()
runCommand = \case
  AnomaIntentsVerify opts -> do
    intentFile <- fromAppPathFile (opts ^. intentsVerifyFile)
    parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty intentFile)
    res <- cellOrFail parsedTerm go
    if
        | res ^. verifyResultValid -> logInfo "Intent is valid" >> exitSuccess
        | otherwise -> logInfo "Intent is invalid" >> exitFailure
    where
      go :: Term Natural -> Sem r VerifyResult
      go t =
        verify
          VerifyInput
            { _verifyIntent = t
            }
