module Commands.Dev.Nockma.Ide.Rules where

import Commands.Base
import Juvix.Compiler.Nockma.Highlight.Doc
import Juvix.Emacs.Render
import Juvix.Emacs.SExp

runCommand :: forall r. (Members AppEffects r) => Sem r ()
runCommand = do
  let (txt, format) = renderEmacs allRules
      ret = Pair (String txt) format
  renderStdOutLn (toPlainText ret)
