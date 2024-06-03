module Juvix.Compiler.Pipeline.Options where

import Juvix.Compiler.Concrete.Translation.ImportScanner
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig
import Juvix.Prelude

data PipelineOptions = PipelineOptions
  { _pipelineImportStrategy :: ImportScanStrategy,
    _pipelineDependenciesConfig :: DependenciesConfig,
    _pipelineNumThreads :: NumThreads
  }

defaultPipelineOptions :: PipelineOptions
defaultPipelineOptions =
  PipelineOptions
    { _pipelineImportStrategy = defaultImportScanStrategy,
      _pipelineDependenciesConfig = defaultDependenciesConfig,
      _pipelineNumThreads = defaultNumThreads
    }

testPipelineOptions :: PipelineOptions
testPipelineOptions = defaultPipelineOptions
