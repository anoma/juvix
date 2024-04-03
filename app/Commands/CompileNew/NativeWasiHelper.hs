module Commands.CompileNew.NativeWasiHelper
  ( module Commands.CompileNew.NativeWasiHelper,
    module Commands.CompileNew.NativeWasiHelper.RuntimeWriter,
  )
where

import Commands.Base
import Commands.CompileNew.CStage
import Commands.CompileNew.CommonOptions
import Commands.CompileNew.NativeWasiHelper.RuntimeWriter
import Commands.Extra.NewCompile
import Juvix.Compiler.Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Extra.Paths

data HelperOptions = HelperOptions
  { _helperCompileCommonOptions :: CompileCommonOptionsMain,
    _helperCStage :: CStage,
    _helperTarget :: Target,
    _helperDefaultOutputFile :: Path Abs File -> Path Abs File -> Path Abs File,
    _helperPrepareRuntime :: forall r. (Members '[App, EmbedIO] r) => Sem r ()
  }

makeLenses ''HelperOptions

helperOutputFile :: (Member App r) => HelperOptions -> Sem r (Path Abs File)
helperOutputFile opts =
  case opts ^. helperCompileCommonOptions . compileOutputFile of
    Just f -> fromAppFile f
    Nothing -> do
      inputFile <- getMainFile (opts ^. helperCompileCommonOptions . compileInputFile)
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return ((opts ^. helperDefaultOutputFile) inputFile baseOutputFile)

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => HelperOptions -> Sem r ()
runCommand opts = do
  let opts' = opts ^. helperCompileCommonOptions
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <-
    set entryPointTarget (Just (opts ^. helperTarget))
      . applyCompileCommonOptions opts'
      <$> getEntryPoint (opts' ^. compileInputFile)
  C.MiniCResult {..} <-
    getRight
      . run
      . runReader entryPoint
      . runError @JuvixError
      $ coreToMiniC (coreRes ^. coreResultModule)
  inputfile <- getMainFile (opts' ^. compileInputFile)
  cFile <- inputCFile inputfile
  writeFileEnsureLn cFile _resultCCode
  outfile <- helperOutputFile opts
  let carg =
        ClangArgs
          { _clangDebug = opts' ^. compileDebug,
            _clangInputFile = cFile,
            _clangOptimizationLevel = opts' ^. compileOptimizationLevel,
            _clangCStage = opts ^. helperCStage,
            _clangOutputFile = outfile
          }
  buildDir <- askBuildDir
  ensureDir buildDir
  ensureDir (juvixIncludeDir buildDir)
  opts ^. helperPrepareRuntime
  clangNativeCompile carg
  where
    inputCFile :: Path Abs File -> Sem r (Path Abs File)
    inputCFile inputFileCompile = do
      buildDir <- askBuildDir
      ensureDir buildDir
      return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))