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
  case Casm.runParser file s of
    Left err -> exitJuvixError (JuvixError err)
    Right (labi, code) ->
      case Casm.validate labi code of
        Left err -> exitJuvixError (JuvixError err)
        Right () -> do
          ep <- getEntryPoint (AppPath (preFileFromAbs file) True)
          tgt <- getTarget (opts ^. compileTarget)
          let entryPoint :: EntryPoint
              entryPoint =
                ep
                  { _entryPointTarget = tgt,
                    _entryPointDebug = opts ^. compileDebug
                  }
          cairoFile <- Compile.outputFile opts file
          r <-
            runReader entryPoint
              . runError @JuvixError
              . casmToCairo
              $ Casm.Result labi code
          res <- getRight r
          liftIO $ JSON.encodeFile (toFilePath cairoFile) res
  where
    getFile :: Sem r (Path Abs File)
    getFile = getMainFile (opts ^. compileInputFile)

    getTarget :: CompileTarget -> Sem r Backend.Target
    getTarget = \case
      TargetCairo -> return Backend.TargetCairo
      TargetWasm32Wasi -> err "WASM"
      TargetNative64 -> err "native"
      TargetCasm -> err "CASM"
      TargetReg -> err "JuvixReg"
      TargetNockma -> err "Nockma"
      TargetAnoma -> err "Anoma"
      TargetTree -> err "JuvixTree"
      TargetGeb -> err "GEB"
      TargetVampIR -> err "VampIR"
      TargetCore -> err "JuvixCore"
      TargetAsm -> err "JuvixAsm"
      where
        err :: Text -> Sem r a
        err tgt = exitMsg (ExitFailure 1) ("error: " <> tgt <> " target not supported for CASM")
