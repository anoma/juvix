module Commands.Typecheck where

import Commands.Base
import Commands.Dev.Internal.Typecheck qualified as Internal
import Commands.Typecheck.Options

runCommand :: (Members '[Embed IO, App] r) => TypecheckOptions -> Sem r ()
runCommand = Internal.runCommand . project
