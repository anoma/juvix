module Juvix.Compiler.Pipeline.Options where

import Juvix.Compiler.Concrete.Translation.ImportScanner
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig
import Juvix.Prelude

data PipelineOptions = PipelineOptions
  { _pipelineImportStrategy :: ImportScanStrategy,
    _pipelineDependenciesConfig :: DependenciesConfig,
    _pipelineNumThreads :: NumThreads,
    _pipelineShowThreadId :: Bool
  }

makeLenses ''PipelineOptions

defaultPipelineOptions :: PipelineOptions
defaultPipelineOptions =
  PipelineOptions
    { _pipelineImportStrategy = defaultImportScanStrategy,
      _pipelineDependenciesConfig = defaultDependenciesConfig,
      _pipelineShowThreadId = False,
      _pipelineNumThreads = defaultNumThreads
    }

-- We need to disable parallel module compilation in the tests until we have
-- project-level locking
testPipelineOptions :: PipelineOptions
testPipelineOptions = defaultPipelineOptions {_pipelineNumThreads = numThreadsOne}
