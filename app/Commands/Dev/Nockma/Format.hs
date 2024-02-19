module Commands.Dev.Nockma.Format where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Format.Options
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

runCommand :: forall r. (Members '[EmbedIO, App] r) => NockmaFormatOptions -> Sem r ()
runCommand opts = do
  afile <- fromAppPathFile file
  parsedTerm <- Nockma.parseTermFile afile
  case parsedTerm of
    Left err -> exitJuvixError (JuvixError err)
    Right t -> putStrLn (ppPrint t)
  where
    file :: AppPath File
    file = opts ^. nockmaFormatFile
