module Commands.Dev.DisplayRoot where

import Commands.Base

runCommand :: Members '[Embed IO, App] r => Sem r ()
runCommand = askRoot >>= say . pack
