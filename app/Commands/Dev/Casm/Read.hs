module Commands.Dev.Casm.Read where

import Commands.Base
import Commands.Dev.Casm.Read.Options
import Juvix.Compiler.Casm.Pretty qualified as Casm
import Juvix.Compiler.Casm.Translation.FromSource qualified as Casm

runCommand :: forall r. (Members '[Embed IO, App] r) => CasmReadOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile (toFilePath afile)
  case Casm.runParser (toFilePath afile) s of
    Left err -> exitJuvixError (JuvixError err)
    Right (_, code) -> renderStdOut (Casm.ppProgram code)
  where
    file :: AppPath File
    file = opts ^. casmReadInputFile
