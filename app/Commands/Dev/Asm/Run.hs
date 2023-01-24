module Commands.Dev.Asm.Run where

import AsmInterpreter
import Commands.Base
import Commands.Dev.Asm.Run.Options
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm

runCommand :: forall r. (Members '[Embed IO, App] r) => AsmRunOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- someBaseToAbs' file
  s <- embed (readFile (toFilePath afile))
  case Asm.runParser (toFilePath afile) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> runAsm (not (opts ^. asmRunNoValidate)) tab
  where
    file :: SomeBase File
    file = opts ^. asmRunInputFile . pathPath
