module Commands.Compile.NativeWasiHelper
  ( module Commands.Compile.NativeWasiHelper,
    module Commands.Compile.NativeWasiHelper.RuntimeWriter,
    module Commands.Extra.Clang.Backend,
  )
where

import Commands.Base
import Commands.Compile.CStage
import Commands.Compile.CommonOptions
import Commands.Compile.NativeWasiHelper.RuntimeWriter
import Commands.Extra.Clang
import Commands.Extra.Clang.Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Extra.Paths

data HelperOptions (k :: InputKind) = HelperOptions
  { _helperCompileCommonOptions :: CompileCommonOptions k,
    _helperCStage :: CStage,
    _helperTarget :: Target,
    _helperClangBackend :: ClangBackend,
    _helperDefaultOutputFile :: Path Abs File -> Path Abs File -> Path Abs File,
    _helperPrepareRuntime :: forall r. (Members '[App, EmbedIO] r) => Sem r ()
  }

makeLenses ''HelperOptions

helperOutputFile :: forall k r. (SingI k, Member App r) => HelperOptions k -> Sem r (Path Abs File)
helperOutputFile opts =
  case opts ^. helperCompileCommonOptions . compileOutputFile of
    Just f -> fromAppFile f
    Nothing -> do
      inputFile <- getMainFileFromInputFileType @k (opts ^. helperCompileCommonOptions . compileInputFile)
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return ((opts ^. helperDefaultOutputFile) inputFile baseOutputFile)

runCommand :: forall r. (Members AppEffects r) => HelperOptions 'InputMain -> Sem r ()
runCommand opts = concreteToC opts >>= fromC opts

concreteToC ::
  forall r.
  (Members AppEffects r) =>
  HelperOptions 'InputMain ->
  Sem r C.MiniCResult
concreteToC HelperOptions {..} = do
  copts <- fromCompileCommonOptionsMain _helperCompileCommonOptions
  let opts' = HelperOptions {_helperCompileCommonOptions = copts, ..}
  r <- runError @JuvixError $ runPipeline opts' (_helperCompileCommonOptions ^. compileInputFile) upToMiniC
  getRight r

fromC :: forall k r. (SingI k, Members '[App, EmbedIO] r) => HelperOptions k -> C.MiniCResult -> Sem r ()
fromC opts cResult = do
  let opts' = opts ^. helperCompileCommonOptions
  inputfile <- getMainFileFromInputFileType @k (opts' ^. compileInputFile)
  cFile <- inputCFile inputfile
  writeFileEnsureLn cFile (cResult ^. C.resultCCode)
  outfile <- helperOutputFile opts
  let carg =
        ClangArgs
          { _clangDebug = opts' ^. compileDebug,
            _clangInputFile = cFile,
            _clangOptimizationLevel = opts' ^. compileOptimizationLevel,
            _clangCStage = opts ^. helperCStage,
            _clangBackend = opts ^. helperClangBackend,
            _clangOutputFile = outfile
          }
  buildDir <- askBuildDir
  ensureDir buildDir
  ensureDir (juvixIncludeDir buildDir)
  opts ^. helperPrepareRuntime
  clangCompile carg

inputCFile :: (Members '[App, EmbedIO] r) => Path Abs File -> Sem r (Path Abs File)
inputCFile inputFileCompile = do
  buildDir <- askBuildDir
  ensureDir buildDir
  return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))

instance EntryPointOptions (HelperOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just (opts ^. helperTarget))
      . applyOptions (opts ^. helperCompileCommonOptions)
