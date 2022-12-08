module Commands.Dev.Asm.Validate where

import Commands.Base
import Commands.Dev.Asm.Validate.Options
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Transformation.Validate qualified as Asm
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm

runCommand :: forall r. Members '[Embed IO, App] r => AsmValidateOptions -> Sem r ()
runCommand opts = do
  s <- embed (readFile file)
  case Asm.runParser file s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> do
      case Asm.validate' tab of
        Just err ->
          exitJuvixError (JuvixError err)
        Nothing ->
          if
              | opts ^. asmValidateNoPrint ->
                  exitMsg ExitSuccess "validation succeeded"
              | otherwise -> do
                  renderStdOut (Asm.ppOutDefault tab tab)
                  exitMsg ExitSuccess ""
  where
    file :: FilePath
    file = opts ^. asmValidateInputFile . pathPath
