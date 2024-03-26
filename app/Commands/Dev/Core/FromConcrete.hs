module Commands.Dev.Core.FromConcrete where

import Commands.Base
import Commands.Dev.DevCompile.Core qualified as Core
import Commands.Dev.DevCompile.Core.Options

runCommand :: forall r. (Members '[EmbedIO, TaggedLock, App] r) => CoreOptions -> Sem r ()
runCommand = Core.runCommand
