module Commands.Dev.Asm.Compile where

import Commands.Base
import Commands.Dev.Asm.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Extra.Paths

runCommand :: forall r. Members '[Embed IO, App] r => AsmCompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- embed (readFile (toFilePath file))
  case Asm.runParser (toFilePath file) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> case run $ runError $ asmToMiniC asmOpts tab of
      Left err -> exitJuvixError err
      Right C.MiniCResult {..} -> do
        root <- askRoot
        ensureDir (root <//> juvixBuildDir)
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
  root <- askRoot
  return (root <//> juvixBuildDir <//> outputMiniCFile)
  where
    outputMiniCFile :: Path Rel File
    outputMiniCFile = replaceExtension' ".c" (filename inputFileCompile)
