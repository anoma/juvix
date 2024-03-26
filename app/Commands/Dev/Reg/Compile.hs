module Commands.Dev.Reg.Compile where

import Commands.Base
import Commands.Dev.Reg.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Aeson qualified as JSON
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Pretty qualified as Casm
import Juvix.Compiler.Reg.Translation.FromSource qualified as Reg
import Juvix.Prelude.Pretty

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- readFile file
  case Reg.runParser file s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> do
      ep <- getEntryPoint (AppPath (preFileFromAbs file) True)
      tgt <- getTarget (opts ^. compileTarget)
      let entryPoint :: EntryPoint
          entryPoint =
            ep
              { _entryPointTarget = tgt,
                _entryPointDebug = opts ^. compileDebug
              }
      case opts ^. compileTarget of
        TargetCasm -> do
          casmFile <- Compile.outputFile opts file
          r <-
            runReader entryPoint
              . runError @JuvixError
              . regToCasm
              $ tab
          Casm.Result {..} <- getRight r
          writeFileEnsureLn casmFile (toPlainText $ Casm.ppProgram _resultCode)
        TargetCairo -> do
          cairoFile <- Compile.outputFile opts file
          r <-
            runReader entryPoint
              . runError @JuvixError
              . regToCairo
              $ tab
          res <- getRight r
          liftIO $ JSON.encodeFile (toFilePath cairoFile) res
        _ ->
          case run $ runReader entryPoint $ runError $ regToMiniC tab of
            Left err -> exitJuvixError err
            Right C.MiniCResult {..} -> do
              buildDir <- askBuildDir
              ensureDir buildDir
              cFile <- inputCFile file
              writeFileEnsureLn cFile _resultCCode
              outfile <- Compile.outputFile opts file
              Compile.runCommand
                opts
                  { _compileInputFile = Just (AppPath (preFileFromAbs cFile) False),
                    _compileOutputFile = Just (AppPath (preFileFromAbs outfile) False)
                  }
  where
    getFile :: Sem r (Path Abs File)
    getFile = getMainFile (opts ^. compileInputFile)

    getTarget :: CompileTarget -> Sem r Backend.Target
    getTarget = \case
      TargetWasm32Wasi -> return Backend.TargetCWasm32Wasi
      TargetNative64 -> return Backend.TargetCNative64
      TargetCasm -> return Backend.TargetCairo
      TargetCairo -> return Backend.TargetCairo
      TargetReg -> err "JuvixReg"
      TargetAnoma -> err "Anoma"
      TargetTree -> err "JuvixTree"
      TargetGeb -> err "GEB"
      TargetVampIR -> err "VampIR"
      TargetCore -> err "JuvixCore"
      TargetAsm -> err "JuvixAsm"
      where
        err :: Text -> Sem r a
        err tgt = exitMsg (ExitFailure 1) ("error: " <> tgt <> " target not supported for JuvixReg")

inputCFile :: (Members '[App] r) => Path Abs File -> Sem r (Path Abs File)
inputCFile inputFileCompile = do
  buildDir <- askBuildDir
  return (buildDir <//> outputMiniCFile)
  where
    outputMiniCFile :: Path Rel File
    outputMiniCFile = replaceExtension' ".c" (filename inputFileCompile)
