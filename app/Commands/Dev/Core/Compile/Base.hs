module Commands.Dev.Core.Compile.Base where

import Commands.Base
import Commands.Dev.Core.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Asm.Pretty qualified as Pretty
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import System.FilePath (takeBaseName)

data PipelineArg = PipelineArg
  { _pipelineArgOptions :: CompileOptions,
    _pipelineArgFile :: Path Abs File,
    _pipelineArgInfoTable :: Core.InfoTable
  }

runCPipeline ::
  forall r.
  (Members '[Embed IO, App] r) =>
  PipelineArg ->
  Sem r ()
runCPipeline PipelineArg {..} = do
  C.MiniCResult {..} <- getRight (run (runError (coreToMiniC asmOpts _pipelineArgInfoTable :: Sem '[Error JuvixError] C.MiniCResult)))
  cFile <- inputCFile _pipelineArgFile
  embed $ TIO.writeFile (toFilePath cFile) _resultCCode
  Compile.runCommand _pipelineArgOptions {_compileInputFile = AppPath (Abs cFile) False}
  where
    asmOpts :: Asm.Options
    asmOpts = Asm.makeOptions (asmTarget (_pipelineArgOptions ^. compileTarget)) (_pipelineArgOptions ^. compileDebug)

    asmTarget :: CompileTarget -> Backend.Target
    asmTarget = \case
      TargetWasm32Wasi -> Backend.TargetCWasm32Wasi
      TargetNative64 -> Backend.TargetCNative64
      TargetGeb -> impossible
      TargetCore -> impossible
      TargetAsm -> impossible

    inputCFile :: Path Abs File -> Sem r (Path Abs File)
    inputCFile inputFileCompile = do
      buildDir <- askBuildDir
      ensureDir buildDir
      return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))

runGebPipeline ::
  forall r.
  (Members '[Embed IO, App] r) =>
  PipelineArg ->
  Sem r ()
runGebPipeline PipelineArg {..} = do
  gebFile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  let spec =
        if
            | _pipelineArgOptions ^. compileTerm -> Geb.OnlyTerm
            | otherwise ->
                Geb.LispPackage
                  Geb.LispPackageSpec
                    { _lispPackageName = fromString $ takeBaseName $ toFilePath gebFile,
                      _lispPackageEntry = "*entry*"
                    }
  Geb.Result {..} <- getRight (run (runError (coreToGeb spec _pipelineArgInfoTable :: Sem '[Error JuvixError] Geb.Result)))
  embed $ TIO.writeFile (toFilePath gebFile) _resultCode

runAsmPipeline :: (Members '[Embed IO, App] r) => PipelineArg -> Sem r ()
runAsmPipeline PipelineArg {..} = do
  asmFile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  let tab' = coreToAsm _pipelineArgInfoTable
      code = Pretty.ppPrint tab' tab'
  embed $ TIO.writeFile (toFilePath asmFile) code
