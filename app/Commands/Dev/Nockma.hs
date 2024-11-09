module Commands.Dev.Nockma where

import Commands.Base
import Commands.Dev.Nockma.Encode as Encode
import Commands.Dev.Nockma.Eval as Eval
import Commands.Dev.Nockma.Format as Format
import Commands.Dev.Nockma.Highlight as Highlight
import Commands.Dev.Nockma.Options
import Commands.Dev.Nockma.Repl as Repl
import Commands.Dev.Nockma.Run as Run

runCommand :: forall r. (Members AppEffects r) => NockmaCommand -> Sem r ()
runCommand = \case
  NockmaRepl opts -> Repl.runCommand opts
  NockmaEval opts -> Eval.runCommand opts
  NockmaFormat opts -> Format.runCommand opts
  NockmaRun opts -> Run.runCommand opts
  NockmaEncode opts -> Encode.runCommand opts
  NockmaHighlight opts -> Highlight.runCommand opts
