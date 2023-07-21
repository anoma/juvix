module Commands.Dev.VM.Compile where

import Commands.Base
import Commands.Dev.VM.Compile.Options
import Commands.Extra.Compile (outputFile)
import Data.ByteString qualified as BS
import Juvix.Compiler.VM.Language qualified as VM
import Juvix.Compiler.VM.Options qualified as VM
import Juvix.Compiler.VM.Serialization.ToVampIR qualified as VM
import Juvix.Compiler.VM.Translation.FromSource qualified as VM

runCommand :: forall r. (Members '[Embed IO, App] r) => CompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- embed (readFile (toFilePath file))
  instrs <- getRight (mapLeft JuvixError (VM.runParser (toFilePath file) s))
  case opts ^. compileTarget of
    TargetWasm32Wasi -> exitMsg (ExitFailure 1) "error: WASM target not supported for JuvixVM"
    TargetNative64 -> exitMsg (ExitFailure 1) "error: native target not supported for JuvixVM"
    TargetGeb -> exitMsg (ExitFailure 1) "error: GEB target not supported for JuvixVM"
    TargetVampIR -> exitMsg (ExitFailure 1) "error: GEB target not supported for JuvixVM"
    TargetCore -> exitMsg (ExitFailure 1) "error: JuvixCore target not supported for JuvixVM"
    TargetAsm -> exitMsg (ExitFailure 1) "error: JuvixAsm target not supported for JuvixVM"
    TargetVampIRVM -> writeVampIR file instrs
  where
    getFile :: Sem r (Path Abs File)
    getFile = getMainFile (opts ^. compileInputFile)

    writeVampIR :: Path Abs File -> [VM.Instruction] -> Sem r ()
    writeVampIR infile instrs = do
      out <- outputFile opts infile
      let r :: Either VM.LabelError ByteString = run $ runError $ VM.serialize opts' instrs
      case r of
        Left err -> exitJuvixError (JuvixError err)
        Right bs ->
          embed (BS.writeFile (toFilePath out) bs)

    opts' =
      VM.defaultOptions
        { VM._optHeapSize = opts ^. compileHeapSize,
          VM._optStackSize = opts ^. compileStackSize,
          VM._optStepsNum = opts ^. compileStepsNum
        }
