module Commands.Org where

import Commands.Org.Options
import Commands.Base

runCommand :: Members '[Embed IO, App] r => OrgOptions -> Sem r ()
runCommand opts = undefined
