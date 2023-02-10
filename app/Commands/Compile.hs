module Commands.Compile where

import Commands.Base
import Commands.Compile.Options
import Commands.Dev.Core.Compile.Base qualified as Compile
import Juvix.Compiler.Core qualified as Core

runCommand :: (Members '[Embed IO, App] r) => CompileOptions -> Sem r ()
runCommand opts@CompileOptions {..} = do
  inputFile <- someBaseToAbs' (_compileInputFile ^. pathPath)
  Core.CoreResult {..} <- runPipeline _compileInputFile upToCore
  let args =
        Compile.PipelineArg
          { _pipelineArgFile = inputFile,
            _pipelineArgOptions = opts,
            _pipelineArgInfoTable = _coreResultTable
          }
  case _compileTarget of
    TargetNative64 -> Compile.runCPipeline args
    TargetWasm32Wasi -> Compile.runCPipeline args
    TargetGeb -> Compile.runGebPipeline args
