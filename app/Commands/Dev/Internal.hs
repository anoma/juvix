module Commands.Dev.Internal where

import Commands.Base
import Commands.Dev.Internal.Options
import Commands.Dev.Internal.Pretty qualified as Pretty
import Commands.Dev.Internal.Typecheck qualified as Typecheck

runCommand :: (Members AppEffects r) => InternalCommand -> Sem r ()
runCommand = \case
  Pretty opts -> Pretty.runCommand opts
  TypeCheck opts -> Typecheck.runCommand opts
