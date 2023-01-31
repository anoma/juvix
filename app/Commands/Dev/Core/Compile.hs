module Commands.Dev.Core.Compile where

import Commands.Base
import Commands.Dev.Core.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

data PipelineArg = PipelineArg {
  _pipelineArgOptions :: CoreCompileOptions,
  _pipelineArgFile :: Path Abs File,
  _pipelineArgInfoTable :: Core.InfoTable
}

makeLenses ''PipelineArg

runCommand :: forall r. (Members '[Embed IO, App] r) => CoreCompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- embed (readFile (toFilePath file))
  tab <- getRight (mapLeft JuvixError (Core.runParserMain file Core.emptyInfoTable s))
  case opts ^. compileTarget of
    TargetGeb -> runGebPipeline (PipelineArg opts file tab)
    TargetWasm32Wasi -> runCPipeline (PipelineArg opts file tab)
    TargetNative64 -> runCPipeline (PipelineArg opts file tab)
    TargetC -> runCPipeline (PipelineArg opts file tab)
  where
    getFile :: Sem r (Path Abs File)
    getFile = someBaseToAbs' (opts ^. compileInputFile . pathPath)

runCPipeline ::
  forall r.
  (Members '[Embed IO, App] r) =>
  PipelineArg ->
  Sem r ()
runCPipeline PipelineArg {..} = do
  C.MiniCResult {..} <- getRight (run (runError (coreToMiniC asmOpts _pipelineArgInfoTable :: Sem '[Error JuvixError] C.MiniCResult)))
  cFile <- inputFile _pipelineArgFile ".c"
  embed $ TIO.writeFile (toFilePath cFile) _resultCCode
  Compile.runCommand _pipelineArgOptions {_compileInputFile = AppPath (Abs cFile) False}
  where
    asmOpts :: Asm.Options
    asmOpts = Asm.makeOptions (asmTarget (_pipelineArgOptions ^. compileTarget)) (_pipelineArgOptions ^. compileDebug)

    asmTarget :: CompileTarget -> Backend.Target
    asmTarget = \case
      TargetWasm32Wasi -> Backend.TargetCWasm32Wasi
      TargetNative64 -> Backend.TargetCNative64
      TargetC -> Backend.TargetCWasm32Wasi
      TargetGeb -> impossible

runGebPipeline ::
  forall r.
  (Members '[Embed IO, App] r) =>
  PipelineArg ->
  Sem r ()
runGebPipeline PipelineArg {..} = do
  Geb.Result {..} <- getRight (run (runError (coreToGeb _pipelineArgInfoTable :: Sem '[Error JuvixError] Geb.Result)))
  gebFile <- inputFile _pipelineArgFile ".geb"
  embed $ TIO.writeFile (toFilePath gebFile) _resultCode

inputFile :: (Members '[Embed IO, App] r) => Path Abs File -> String -> Sem r (Path Abs File)
inputFile inputFileCompile ext = do
  buildDir <- askBuildDir
  ensureDir buildDir
  return (buildDir <//> replaceExtension' ext (filename inputFileCompile))
