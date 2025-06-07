module Commands.Dev.PlainMarkdown where

import Commands.Base
import Commands.Dev.PlainMarkdown.Format qualified as Format
import Commands.Dev.PlainMarkdown.Options

runCommand :: forall r. (Members AppEffects r) => PlainMarkdownCommand -> Sem r ()
runCommand = \case
  Format opts -> Format.runCommand opts
