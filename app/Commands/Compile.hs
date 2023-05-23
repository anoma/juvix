module Commands.Compile where

import Commands.Base
import Commands.Compile.Options
import Commands.Dev.Core.Compile.Base qualified as Compile
import Commands.Extra.Compile qualified as Compile
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames qualified as Core

runCommand :: (Members '[Embed IO, App] r) => CompileOptions -> Sem r ()
runCommand opts@CompileOptions {..} = do
  inputFile <- getMainFile _compileInputFile
  Core.CoreResult {..} <- runPipeline (AppPath (preFileFromAbs inputFile) True) upToCore
  let arg =
        Compile.PipelineArg
          { _pipelineArgFile = inputFile,
            _pipelineArgOptions = opts,
            _pipelineArgInfoTable = _coreResultTable
          }
  case _compileTarget of
    TargetNative64 -> Compile.runCPipeline arg
    TargetWasm32Wasi -> Compile.runCPipeline arg
    TargetGeb -> Compile.runGebPipeline arg
    TargetVampIR -> Compile.runVampIRPipeline arg
    TargetCore -> writeCoreFile arg
    TargetAsm -> Compile.runAsmPipeline arg

writeCoreFile :: (Members '[Embed IO, App] r) => Compile.PipelineArg -> Sem r ()
writeCoreFile pa@Compile.PipelineArg {..} = do
  entryPoint <- Compile.getEntry pa
  coreFile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  r <- runReader entryPoint $ runError @JuvixError $ Core.toEval _pipelineArgInfoTable
  case r of
    Left e -> exitJuvixError e
    Right tab ->
      embed $ TIO.writeFile (toFilePath coreFile) (show $ Core.ppOutDefault (Core.disambiguateNames tab))
