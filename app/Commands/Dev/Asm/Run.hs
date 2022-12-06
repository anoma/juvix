module Commands.Dev.Asm.Run where

import Commands.Base
import Commands.Dev.Asm.Run.Options
import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Asm.Error qualified as Asm
import Juvix.Compiler.Asm.Extra qualified as Asm
import Juvix.Compiler.Asm.Interpreter qualified as Asm
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Transformation.Validate qualified as Asm
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm

runCommand :: forall r. Members '[Embed IO, App] r => AsmRunOptions -> Sem r ()
runCommand opts = do
  s <- embed (readFile file)
  case Asm.runParser file s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab ->
      let v = if opts ^. asmRunNoValidate then Nothing else Asm.validate' tab
       in case v of
            Just err ->
              exitJuvixError (JuvixError err)
            Nothing ->
              case tab ^. Asm.infoMainFunction of
                Just sym -> do
                  r <- doRun tab (Asm.getFunInfo tab sym)
                  case r of
                    Left err ->
                      exitJuvixError (JuvixError err)
                    Right Asm.ValVoid ->
                      return ()
                    Right val -> do
                      renderStdOut (Asm.ppOut (Asm.defaultOptions tab) val)
                      embed (putStrLn "")
                Nothing ->
                  exitMsg (ExitFailure 1) "no 'main' function"
  where
    file :: FilePath
    file = opts ^. asmRunInputFile . pathPath

    doRun ::
      Asm.InfoTable ->
      Asm.FunctionInfo ->
      Sem r (Either Asm.AsmError Asm.Val)
    doRun tab funInfo =
      embed $ Asm.catchRunErrorIO (Asm.runCodeIO tab funInfo)
