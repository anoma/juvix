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
  file <- getMainFile (Just (opts ^. compileInputFile))
  s <- readFile file
  tab <- fromRightGenericError (Reg.runParser file s)
  ep <- getEntryPoint (Just (opts ^. compileInputFile))
  tgt <- getTarget (opts ^. compileTarget)
  let entryPoint :: EntryPoint
      entryPoint =
        ep
          { _entryPointTarget = Just tgt,
            _entryPointDebug = opts ^. compileDebug
          }
  case opts ^. compileTarget of
    AppTargetCasm -> do
      casmFile <- Compile.outputFile opts
      r <-
        runReader entryPoint
          . runError @JuvixError
          . regToCasm
          $ tab
      Casm.Result {..} <- getRight r
      writeFileEnsureLn casmFile (toPlainText $ Casm.ppProgram _resultCode)
    AppTargetCairo -> do
      cairoFile <- Compile.outputFile opts
      r <-
        runReader entryPoint
          . runError @JuvixError
          . regToCairo
          $ tab
      res <- getRight r
      liftIO $ JSON.encodeFile (toFilePath cairoFile) res
    _ -> do
      C.MiniCResult {..} <-
        fromRightJuvixError
          . run
          . runReader entryPoint
          . runError
          $ regToMiniC tab
      buildDir <- askBuildDir
      ensureDir buildDir
      cFile <- inputCFile file
      writeFileEnsureLn cFile _resultCCode
      outfile <- Compile.outputFile opts
      Compile.runCommand
        opts
          { _compileInputFile = AppPath (preFileFromAbs cFile) False,
            _compileOutputFile = Just (AppPath (preFileFromAbs outfile) False)
          }
  where
    getTarget :: CompileTarget -> Sem r Backend.Target
    getTarget = \case
      AppTargetWasm32Wasi -> return Backend.TargetCWasm32Wasi
      AppTargetNative64 -> return Backend.TargetCNative64
      AppTargetCasm -> return Backend.TargetCairo
      AppTargetCairo -> return Backend.TargetCairo
      AppTargetReg -> err "JuvixReg"
      AppTargetAnoma -> err "Anoma"
      AppTargetTree -> err "JuvixTree"
      AppTargetGeb -> err "GEB"
      AppTargetVampIR -> err "VampIR"
      AppTargetCore -> err "JuvixCore"
      AppTargetAsm -> err "JuvixAsm"
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
