module Commands.Dev.Nockma.Format where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Format.Options
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members AppEffects r) => NockmaFormatOptions -> Sem r ()
runCommand opts = do
  afile <- fromAppPathFile file
  parsedTerm <- runAppError @JuvixError (Nockma.parseTermFile afile)
  putStrLn (ppPrint parsedTerm)
  where
    file :: AppPath File
    file = opts ^. nockmaFormatFile
