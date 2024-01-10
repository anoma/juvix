module Commands.Dev.Asm.Compile where

import Commands.Base
import Commands.Dev.Asm.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromAsm qualified as Nockma

runCommand :: forall r. (Members '[Embed IO, App, TaggedLock] r) => AsmCompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- readFile (toFilePath file)
  case Asm.runParser (toFilePath file) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> do
      case opts ^. compileTarget of
        TargetNockma -> do
          c <- runError (Nockma.fromAsmTable tab) >>= either exitJuvixError return
          let outputCell = Nockma.TermCell c
              outputText = Nockma.ppPrintOpts nockmaOpts outputCell
          embed @IO (writeFileEnsureLn (toFilePath (replaceExtension' ".nockma" file)) outputText)
        _ -> do
          ep <- getEntryPoint (AppPath (preFileFromAbs file) True)
          tgt <- getTarget (opts ^. compileTarget)
          let entryPoint :: EntryPoint
              entryPoint =
                ep
                  { _entryPointTarget = tgt,
                    _entryPointDebug = opts ^. compileDebug
                  }
          case run $ runReader entryPoint $ runError $ asmToMiniC tab of
            Left err -> exitJuvixError err
            Right C.MiniCResult {..} -> do
              buildDir <- askBuildDir
              ensureDir buildDir
              cFile <- inputCFile file
              embed @IO $ writeFileEnsureLn (toFilePath cFile) _resultCCode
              outfile <- Compile.outputFile opts file
              Compile.runCommand
                opts
                  { _compileInputFile = Just (AppPath (preFileFromAbs cFile) False),
                    _compileOutputFile = Just (AppPath (preFileFromAbs outfile) False)
                  }
  where
    getFile :: Sem r (Path Abs File)
    getFile = getMainFile (opts ^. compileInputFile)

    nockmaOpts :: Nockma.Options
    nockmaOpts = Nockma.defaultOptions {Nockma._optIgnoreHints = not (opts ^. compileNockmaUsePrettySymbols)}

    getTarget :: CompileTarget -> Sem r Backend.Target
    getTarget = \case
      TargetWasm32Wasi -> return Backend.TargetCWasm32Wasi
      TargetNative64 -> return Backend.TargetCNative64
      TargetNockma -> return Backend.TargetNockma
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
