module Commands.Dev.Internal where

import Commands.Base
import Commands.Dev.Internal.Options
import Commands.Dev.Internal.Pretty qualified as Pretty
import Commands.Dev.Internal.Reachability qualified as Reachability
import Commands.Dev.Internal.Typecheck qualified as Typecheck

runCommand :: (Members '[Embed IO, App, TaggedLock] r) => InternalCommand -> Sem r ()
runCommand = \case
  Pretty opts -> Pretty.runCommand opts
  TypeCheck opts -> Typecheck.runCommand opts
  Reachability opts -> Reachability.runCommand opts
