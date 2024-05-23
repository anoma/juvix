module Juvix.Compiler.Pipeline.Options where

import Juvix.Compiler.Concrete.Translation.ImportScanner
import Juvix.Compiler.Pipeline.Loader.PathResolver.DependenciesConfig
import Juvix.Prelude

data PipelineOptions = PipelineOptions
  { _pipelineImportStrategy :: ImportScanStrategy,
    _pipelineDependenciesConfig :: DependenciesConfig,
    _pipelineNumJobs :: NumJobs
  }

defaultPipelineOptions :: PipelineOptions
defaultPipelineOptions =
  PipelineOptions
    { _pipelineImportStrategy = defaultImportScanStrategy,
      _pipelineDependenciesConfig = defaultDependenciesConfig,
      _pipelineNumJobs = defaultNumJobs
    }
