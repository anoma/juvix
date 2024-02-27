module RegInterpreter where

import App
import CommonOptions
import Juvix.Compiler.Reg.Data.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Interpreter qualified as Reg
import Juvix.Compiler.Reg.Pretty qualified as Reg

runReg :: forall r. (Members '[EmbedIO, App] r) => Reg.InfoTable -> Sem r ()
runReg tab =
  case tab ^. Reg.infoMainFunction of
    Just sym -> do
      r <- doRun tab (Reg.lookupFunInfo tab sym)
      case r of
        Left err ->
          exitJuvixError (JuvixError err)
        Right Reg.ValVoid ->
          return ()
        Right val -> do
          renderStdOut (Reg.ppOut (Reg.defaultOptions tab) val)
          putStrLn ""
    Nothing ->
      exitMsg (ExitFailure 1) "no 'main' function"
  where
    doRun ::
      Reg.InfoTable ->
      Reg.FunctionInfo ->
      Sem r (Either Reg.RegError Reg.Val)
    doRun tab' funInfo = runError $ Reg.runFunctionIO stdin stdout tab' [] funInfo
