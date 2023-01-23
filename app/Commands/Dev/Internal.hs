module Commands.Dev.Internal where

import Commands.Base
import Commands.Dev.Internal.Arity qualified as Arity
import Commands.Dev.Internal.CoreEval qualified as InternalCoreEval
import Commands.Dev.Internal.Options
import Commands.Dev.Internal.Pretty qualified as InternalPretty
import Commands.Dev.Internal.Typecheck qualified as InternalTypecheck

runCommand :: (Members '[Embed IO, App] r) => InternalCommand -> Sem r ()
runCommand = \case
  Pretty opts -> InternalPretty.runCommand opts
  Arity opts -> Arity.runCommand opts
  TypeCheck opts -> InternalTypecheck.runCommand opts
  CoreEval opts -> InternalCoreEval.runCommand opts
