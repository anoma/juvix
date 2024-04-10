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
import Commands.Extra.NewCompile
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

helperOutputFile :: (Member App r) => HelperOptions 'InputMain -> Sem r (Path Abs File)
helperOutputFile opts =
  case opts ^. helperCompileCommonOptions . compileOutputFile of
    Just f -> fromAppFile f
    Nothing -> do
      inputFile <- getMainFile (opts ^. helperCompileCommonOptions . compileInputFile)
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return ((opts ^. helperDefaultOutputFile) inputFile baseOutputFile)

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => HelperOptions 'InputMain -> Sem r ()
runCommand opts = do
  let opts' = opts ^. helperCompileCommonOptions
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <-
    applyOptions opts
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
            _clangBackend = opts ^. helperClangBackend,
            _clangOutputFile = outfile
          }
  buildDir <- askBuildDir
  ensureDir buildDir
  ensureDir (juvixIncludeDir buildDir)
  opts ^. helperPrepareRuntime
  clangCompile carg
  where
    inputCFile :: Path Abs File -> Sem r (Path Abs File)
    inputCFile inputFileCompile = do
      buildDir <- askBuildDir
      ensureDir buildDir
      return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))

instance EntryPointOptions (HelperOptions k) where
  applyOptions opts =
    set entryPointTarget (Just (opts ^. helperTarget))
      . applyOptions (opts ^. helperCompileCommonOptions)
