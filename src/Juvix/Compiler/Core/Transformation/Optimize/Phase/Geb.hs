module Juvix.Compiler.Core.Transformation.Optimize.Phase.Geb where

import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.Phase.Main qualified as Main

optimize :: (Member (Reader CoreOptions) r) => Module -> Sem r Module
optimize = withOptimizationLevel 1 Main.optimize
