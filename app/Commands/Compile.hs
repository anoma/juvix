module Commands.Compile where

import Commands.Base
import Commands.Compile.Options
import Commands.Dev.Core.Compile.Base qualified as Compile
import Commands.Extra.Compile qualified as Compile
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames qualified as Core

runCommand :: (Members '[EmbedIO, App, TaggedLock] r) => CompileOptionsMain -> Sem r ()
runCommand opts@CompileOptions {..} = do
  opts' <- fromCompileOptionsMain opts
  Core.CoreResult {..} <- runPipeline _compileInputFile upToCore
  let arg =
        Compile.PipelineArg
          { _pipelineArgOptions = opts',
            _pipelineArgModule = _coreResultModule
          }
  case _compileTarget of
    TargetNative64 -> Compile.runCPipeline arg
    TargetWasm32Wasi -> Compile.runCPipeline arg
    TargetGeb -> Compile.runGebPipeline arg
    TargetVampIR -> Compile.runVampIRPipeline arg
    TargetCore -> writeCoreFile arg
    TargetTree -> Compile.runTreePipeline arg
    TargetAsm -> Compile.runAsmPipeline arg
    TargetReg -> Compile.runRegPipeline arg
    TargetAnoma -> Compile.runAnomaPipeline arg
    TargetCasm -> Compile.runCasmPipeline arg
    TargetCairo -> Compile.runCairoPipeline arg

writeCoreFile :: (Members '[EmbedIO, App, TaggedLock] r) => Compile.PipelineArg -> Sem r ()
writeCoreFile pa@Compile.PipelineArg {..} = do
  entryPoint <- Compile.getEntry pa
  coreFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      $ Core.toStored _pipelineArgModule
  md <- fromRightJuvixError r
  let txt = show (Core.ppOutDefault (Core.disambiguateNames md ^. Core.moduleInfoTable))
  writeFileEnsureLn coreFile txt
