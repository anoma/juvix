module Juvix.Compiler.Core.Transformation.Optimize.Phase.Lifting where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.Inlining
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding

optimize :: Member (Reader CoreOptions) r => InfoTable -> Sem r InfoTable
optimize tab = return tab
