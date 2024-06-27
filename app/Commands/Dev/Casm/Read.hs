module Commands.Dev.Casm.Read where

import Commands.Base
import Commands.Dev.Casm.Read.Options
import Juvix.Compiler.Casm.Data.InputInfo qualified as Casm
import Juvix.Compiler.Casm.Extra.LabelInfo qualified as Casm
import Juvix.Compiler.Casm.Interpreter qualified as Casm
import Juvix.Compiler.Casm.Pretty qualified as Casm.Pretty
import Juvix.Compiler.Casm.Transformation qualified as Casm
import Juvix.Compiler.Casm.Translation.FromSource qualified as Casm
import Juvix.Compiler.Casm.Validate qualified as Casm

runCommand :: forall r. (Members '[EmbedIO, App] r) => CasmReadOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile afile
  case Casm.runParser afile s of
    Left err -> exitJuvixError (JuvixError err)
    Right (labi, code) ->
      case Casm.validate labi code of
        Left err -> exitJuvixError (JuvixError err)
        Right () -> do
          r <-
            runError @JuvixError
              . runReader Casm.defaultOptions
              $ (Casm.applyTransformations (project opts ^. casmReadTransformations) code)
          case r of
            Left err -> exitJuvixError (JuvixError err)
            Right code' -> do
              unless (project opts ^. casmReadNoPrint) $
                renderStdOut (Casm.Pretty.ppProgram code')
              doRun code'
  where
    file :: AppPath File
    file = opts ^. casmReadInputFile

    doRun :: Casm.Code -> Sem r ()
    doRun code'
      | project opts ^. casmReadRun = do
          putStrLn "--------------------------------"
          putStrLn "|            Run               |"
          putStrLn "--------------------------------"
          let labi = Casm.computeLabelInfo code'
              inputInfo = Casm.InputInfo mempty
          print (Casm.runCode inputInfo labi code')
      | otherwise = return ()
