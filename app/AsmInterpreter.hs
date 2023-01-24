module AsmInterpreter where

import App
import CommonOptions
import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Asm.Extra qualified as Asm
import Juvix.Compiler.Asm.Interpreter qualified as Asm
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Transformation.Validate qualified as Asm

runAsm :: forall r. (Members '[Embed IO, App] r) => Bool -> Asm.InfoTable -> Sem r ()
runAsm bValidate tab =
  let v = if bValidate then Asm.validate' tab else Nothing
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
    doRun ::
      Asm.InfoTable ->
      Asm.FunctionInfo ->
      Sem r (Either Asm.AsmError Asm.Val)
    doRun tab' funInfo =
      embed $ Asm.catchRunErrorIO (Asm.runCodeIO tab' funInfo)
