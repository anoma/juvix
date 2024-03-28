module Commands.CompileNew.Native where

import Commands.Base
import Commands.CompileNew.Native.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => NativeOptions -> Sem r ()
runCommand opts = do
  let opts' = opts ^. nativeCompileCommonOptions
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <- getEntryPoint (opts' ^. compileInputFile)
  C.MiniCResult {..} <-
    getRight
      . run
      . runReader entryPoint
      . runError @JuvixError
      $ coreToMiniC (coreRes ^. coreResultModule)
  inputfile <- getMainFile (opts' ^. compileInputFile)
  cFile <- inputCFile inputfile
  writeFileEnsureLn cFile _resultCCode
  outfile <- nativeOutputFile opts
  let carg =
        ClangArgs
          { _clangDebug = opts' ^. compileDebug,
            _clangInputFile = outfile,
            _clangOptimizationLevel = opts' ^. compileOptimizationLevel,
            _clangCStage = opts ^. nativeCStage,
            _clangOutputFile = outfile
          }
  clangNativeCompile carg
  where
    inputCFile :: Path Abs File -> Sem r (Path Abs File)
    inputCFile inputFileCompile = do
      buildDir <- askBuildDir
      ensureDir buildDir
      return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))
