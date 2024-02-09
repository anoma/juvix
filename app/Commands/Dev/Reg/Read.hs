module Commands.Dev.Reg.Read where

import Commands.Base
import Commands.Dev.Reg.Read.Options
import Juvix.Compiler.Reg.Pretty qualified as Reg
import Juvix.Compiler.Reg.Translation.FromSource qualified as Reg

runCommand :: forall r. (Members '[Embed IO, App] r) => RegReadOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile (toFilePath afile)
  case Reg.runParser (toFilePath afile) s of
    Left err ->
      exitJuvixError (JuvixError err)
    Right tab ->
      renderStdOut (Reg.ppOutDefault tab tab)
  where
    file :: AppPath File
    file = opts ^. regReadInputFile
