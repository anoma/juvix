module Commands.Dev.Nockma.FromAsm where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.FromAsm.Options

runCommand :: forall r. (Members '[Embed IO, App] r) => NockmaFromAsmOptions -> Sem r ()
runCommand _opts = return ()
