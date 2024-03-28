module Commands.CompileNew.Native where

import Commands.Base
import Commands.CompileNew.Native.Options
import Commands.Dev.Core.Compile.Base qualified as Compile
import Commands.Extra.NewCompile
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => NativeOptions -> Sem r ()
runCommand opts = do
  coreRes <- fromCompileCommonOptionsMain (opts ^. nativeCompileCommonOptions) >>= compileToCore
  let arg =
        Compile.PipelineArg
          { _pipelineArgOptions = undefined,
            _pipelineArgModule = coreRes ^. coreResultModule
          }

  Compile.runCPipeline arg
