module Juvix.Compiler.Pipeline.Result where

import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude

data PipelineResult a = PipelineResult
  { _pipelineResult :: a,
    _pipelineResultImports :: Store.ModuleTable
  }

makeLenses ''PipelineResult
