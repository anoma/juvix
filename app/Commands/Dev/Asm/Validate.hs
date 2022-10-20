module Commands.Dev.Asm.Validate where

import Commands.Base
import Commands.Dev.Asm.Validate.Options
import Juvix.Compiler.Asm.Extra qualified as Asm
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm

runCommand :: forall r. Members '[Embed IO, App] r => AsmValidateOptions -> Sem r ()
runCommand opts = do
  s <- embed (readFile file)
  case Asm.runParser file s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> do
      case Asm.validate tab of
        Just err ->
          exitJuvixError (JuvixError err)
        Nothing ->
          exitMsg ExitSuccess "validation succeeded"
  where
    file :: FilePath
    file = opts ^. asmValidateInputFile . pathPath
