module Commands.Dev.Core.Compile where

import Commands.Base
import Commands.Dev.Core.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. Members '[Embed IO, App] r => CoreCompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- embed (readFile (toFilePath file))
  tab <- getRight (mapLeft JuvixError (Core.runParserMain (toFilePath file) Core.emptyInfoTable s))
  C.MiniCResult {..} <- getRight (run (runError (coreToMiniC asmOpts tab :: Sem '[Error JuvixError] C.MiniCResult)))
  buildDir <- askBuildDir
  ensureDir buildDir
  cFile <- inputCFile file
  embed $ TIO.writeFile (toFilePath cFile) _resultCCode
  Compile.runCommand opts {_compileInputFile = AppPath (Abs cFile) False}
  where
    getFile :: Sem r (Path Abs File)
    getFile = someBaseToAbs' (opts ^. compileInputFile . pathPath)

    asmOpts :: Asm.Options
    asmOpts = Asm.makeOptions (asmTarget (opts ^. compileTarget)) (opts ^. compileDebug)

    asmTarget :: CompileTarget -> Backend.Target
    asmTarget = \case
      TargetWasm32Wasi -> Backend.TargetCWasm32Wasi
      TargetNative64 -> Backend.TargetCNative64
      TargetC -> Backend.TargetCWasm32Wasi

inputCFile :: Members '[App] r => Path Abs File -> Sem r (Path Abs File)
inputCFile inputFileCompile = do
  buildDir <- askBuildDir
  return (buildDir <//> outputMiniCFile)
  where
    outputMiniCFile :: Path Rel File
    outputMiniCFile = replaceExtension' ".c" (filename inputFileCompile)
