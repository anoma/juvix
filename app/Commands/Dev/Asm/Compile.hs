module Commands.Dev.Asm.Compile where

import Commands.Base
import Commands.Dev.Asm.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C

runCommand :: forall r. (Members '[Embed IO, App] r) => AsmCompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  ep <- getEntryPoint (AppPath (preFileFromAbs file) True)
  tgt <- getTarget (opts ^. compileTarget)
  let entryPoint :: EntryPoint
      entryPoint =
        ep
          { _entryPointTarget = tgt,
            _entryPointDebug = opts ^. compileDebug
          }
  s <- embed (readFile (toFilePath file))
  case Asm.runParser (toFilePath file) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> do
      case run $ runReader entryPoint $ runError $ asmToMiniC tab of
        Left err -> exitJuvixError err
        Right C.MiniCResult {..} -> do
          buildDir <- askBuildDir
          ensureDir buildDir
          cFile <- inputCFile file
          embed $ TIO.writeFile (toFilePath cFile) _resultCCode
          Compile.runCommand opts {_compileInputFile = Just (AppPath (preFileFromAbs cFile) False)}
  where
    getFile :: Sem r (Path Abs File)
    getFile = getMainFile (opts ^. compileInputFile)

    getTarget :: CompileTarget -> Sem r Backend.Target
    getTarget = \case
      TargetWasm32Wasi -> return Backend.TargetCWasm32Wasi
      TargetNative64 -> return Backend.TargetCNative64
      TargetGeb -> exitMsg (ExitFailure 1) "error: GEB target not supported for JuvixAsm"
      TargetVampIR -> exitMsg (ExitFailure 1) "error: VampIR target not supported for JuvixAsm"
      TargetCore -> exitMsg (ExitFailure 1) "error: JuvixCore target not supported for JuvixAsm"
      TargetAsm -> exitMsg (ExitFailure 1) "error: JuvixAsm target not supported for JuvixAsm"

inputCFile :: (Members '[App] r) => Path Abs File -> Sem r (Path Abs File)
inputCFile inputFileCompile = do
  buildDir <- askBuildDir
  return (buildDir <//> outputMiniCFile)
  where
    outputMiniCFile :: Path Rel File
    outputMiniCFile = replaceExtension' ".c" (filename inputFileCompile)
