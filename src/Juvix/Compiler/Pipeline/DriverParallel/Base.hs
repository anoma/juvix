module Juvix.Compiler.Pipeline.DriverParallel.Base where

import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Prelude

type ImportsAccess = Reader (HashMap (Path Abs File) (PipelineResult Store.ModuleInfo))
