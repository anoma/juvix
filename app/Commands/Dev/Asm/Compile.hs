module Commands.Dev.Asm.Compile where

import Commands.Base
import Commands.Dev.Asm.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Options qualified as Asm
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C

runCommand :: forall r. (Members '[Embed IO, App] r) => AsmCompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- embed (readFile (toFilePath file))
  case Asm.runParser (toFilePath file) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> do
      tgt <- asmTarget (opts ^. compileTarget)
      case run $ runError $ asmToMiniC (asmOpts tgt) tab of
        Left err -> exitJuvixError err
        Right C.MiniCResult {..} -> do
          buildDir <- askBuildDir
          ensureDir buildDir
          cFile <- inputCFile file
          embed $ TIO.writeFile (toFilePath cFile) _resultCCode
          Compile.runCommand opts {_compileInputFile = AppPath (Abs cFile) False}
  where
    getFile :: Sem r (Path Abs File)
    getFile = someBaseToAbs' (opts ^. compileInputFile . pathPath)

    asmOpts :: Backend.Target -> Asm.Options
    asmOpts tgt = Asm.makeOptions tgt (opts ^. compileDebug)

    asmTarget :: CompileTarget -> Sem r Backend.Target
    asmTarget = \case
      TargetWasm32Wasi -> return Backend.TargetCWasm32Wasi
      TargetNative64 -> return Backend.TargetCNative64
      TargetGeb -> exitMsg (ExitFailure 1) "error: GEB target not supported for JuvixAsm"
      TargetCore -> exitMsg (ExitFailure 1) "error: JuvixCore target not supported for JuvixAsm"
      TargetAsm -> exitMsg (ExitFailure 1) "error: JuvixAsm target not supported for JuvixAsm"

inputCFile :: (Members '[App] r) => Path Abs File -> Sem r (Path Abs File)
inputCFile inputFileCompile = do
  buildDir <- askBuildDir
  return (buildDir <//> outputMiniCFile)
  where
    outputMiniCFile :: Path Rel File
    outputMiniCFile = replaceExtension' ".c" (filename inputFileCompile)
