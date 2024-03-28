module Commands.CompileNew.Wasi where

import Commands.Base
import Commands.CompileNew.Wasi.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context

-- TODO reduce duplication with Native
runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => WasiOptions -> Sem r ()
runCommand opts = do
  let opts' = opts ^. wasiCompileCommonOptions
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
  outfile <- wasiOutputFile opts
  let carg =
        ClangArgs
          { _clangDebug = opts' ^. compileDebug,
            _clangInputFile = outfile,
            _clangOptimizationLevel = opts' ^. compileOptimizationLevel,
            _clangCStage = opts ^. wasiCStage,
            _clangOutputFile = outfile
          }
  clangWasiCompile carg
  where
    inputCFile :: Path Abs File -> Sem r (Path Abs File)
    inputCFile inputFileCompile = do
      buildDir <- askBuildDir
      ensureDir buildDir
      return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))
