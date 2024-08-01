module Commands.Dev.Latex.GetJuvixSty where

import Commands.Base
import Commands.Dev.Latex.GetJuvixSty.Options
import Juvix.Extra.Paths

runCommand :: (Members AppEffects r) => GetJuvixStyOptions -> Sem r ()
runCommand _ = renderStdOutRaw juvixSty
