module Commands.Dev.Casm.Compile where

import Commands.Base
import Commands.Dev.Casm.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Data.Aeson qualified as JSON
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Translation.FromSource qualified as Casm
import Juvix.Compiler.Casm.Validate qualified as Casm

runCommand :: forall r. (Members '[EmbedIO, App, TaggedLock] r) => CompileOptions -> Sem r ()
runCommand opts = do
  file <- getFile
  s <- readFile file
  (labi, code) <- fromRightGenericError (Casm.runParser file s)
  () <- fromRightGenericError (Casm.validate labi code)
  ep <- getEntryPoint (Just (opts ^. compileInputFile))
  tgt <- getTarget (opts ^. compileTarget)
  let entryPoint :: EntryPoint
      entryPoint =
        ep
          { _entryPointTarget = Just tgt,
            _entryPointDebug = opts ^. compileDebug
          }
  cairoFile <- Compile.outputFile opts
  r <-
    runReader entryPoint
      . runError @JuvixError
      . casmToCairo
      $ Casm.Result
        { _resultLabelInfo = labi,
          _resultCode = code,
          _resultBuiltins = [],
          _resultOutputSize = 1
        }
  res <- getRight r
  liftIO $ JSON.encodeFile (toFilePath cairoFile) res
  where
    getFile :: Sem r (Path Abs File)
    getFile = getMainFile (Just (opts ^. compileInputFile))

    getTarget :: CompileTarget -> Sem r Backend.Target
    getTarget = \case
      AppTargetCairo -> return Backend.TargetCairo
      AppTargetWasm32Wasi -> err "WASM"
      AppTargetNative64 -> err "native"
      AppTargetCasm -> err "CASM"
      AppTargetReg -> err "JuvixReg"
      AppTargetAnoma -> err "Anoma"
      AppTargetTree -> err "JuvixTree"
      AppTargetGeb -> err "GEB"
      AppTargetVampIR -> err "VampIR"
      AppTargetCore -> err "JuvixCore"
      AppTargetAsm -> err "JuvixAsm"
      AppTargetRiscZeroRust -> err "RISC0 Rust"
      where
        err :: Text -> Sem r a
        err tgt = exitMsg (ExitFailure 1) ("error: " <> tgt <> " target not supported for CASM")
