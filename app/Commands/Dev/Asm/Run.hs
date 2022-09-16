module Commands.Dev.Asm.Run where

import Commands.Base
import Commands.Dev.Asm.Run.Options
import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Asm.Error qualified as Asm
import Juvix.Compiler.Asm.Extra qualified as Asm
import Juvix.Compiler.Asm.Interpreter qualified as Asm
import Juvix.Compiler.Asm.Interpreter.Runtime qualified as Asm
import Juvix.Compiler.Asm.Language qualified as Asm
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm

doRun ::
  forall r.
  Members '[Embed IO, App] r =>
  Asm.InfoTable ->
  Asm.Code ->
  Sem r (Either Asm.AsmError Asm.Val)
doRun tab code =
  embed $ Asm.catchRunErrorIO (Asm.runCodeIO tab code)

runCommand :: forall r. Members '[Embed IO, App] r => AsmRunOptions -> Sem r ()
runCommand opts = do
  s <- embed (readFile file)
  case Asm.runParser "" file s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab ->
      case tab ^. Asm.infoMainFunction of
        Just sym -> do
          let code = Asm.getFunInfo tab sym ^. Asm.functionCode
          r <- doRun tab code
          case r of
            Left err ->
              exitJuvixError (JuvixError err)
            Right (Asm.ValUnit (Asm.Unit False)) ->
              return ()
            Right val -> do
              renderStdOut (Asm.ppOut (Asm.defaultOptions tab) val)
              embed (putStrLn "")
        Nothing ->
          exitMsg (ExitFailure 1) "no main function"
  where
    file :: FilePath
    file = opts ^. asmRunInputFile . pathPath
