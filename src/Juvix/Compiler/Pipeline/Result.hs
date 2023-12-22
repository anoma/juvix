module Juvix.Compiler.Pipeline.Result where

import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude

data PipelineResult a = PipelineResult
  { _pipelineResult :: a,
    -- | Transitive imports. The imports table contains all dependencies,
    -- transitively. E.g., if module M imports A but not B, but A imports B,
    -- then still both A and B will be in the imports table in the pipeline
    -- result for processing M.
    _pipelineResultImports :: Store.ModuleTable,
    -- | True if the module had to be recompiled. False if the module was loaded
    -- from disk.
    _pipelineResultChanged :: Bool
  }

makeLenses ''PipelineResult
