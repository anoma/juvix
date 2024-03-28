module Commands.Dev.Core.Compile.BaseNew where

import Commands.Base
import Commands.CompileNew.CommonOptions
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Core.Data.Module qualified as Core

data PipelineArg = PipelineArg
  { _pipelineArgOptions :: CompileCommonOptions,
    _pipelineArgTarget :: Backend.Target,
    _pipelineArgModule :: Core.Module
  }

getEntry :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r EntryPoint
getEntry PipelineArg {..} = do
  ep <- getEntryPoint (Just (_pipelineArgOptions ^. compileInputFile))
  return $
    ep
      { _entryPointTarget = _pipelineArgTarget,
        _entryPointDebug = _pipelineArgOptions ^. compileDebug,
        _entryPointOptimizationLevel = fromMaybe defaultOptLevel (_pipelineArgOptions ^. compileOptimizationLevel),
        _entryPointInliningDepth = _pipelineArgOptions ^. compileInliningDepth
      }
  where
    defaultOptLevel :: Int
    defaultOptLevel
      | _pipelineArgOptions ^. compileDebug = 0
      | otherwise = defaultOptimizationLevel
