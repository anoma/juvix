module Juvix.Compiler.Core.Transformation.Optimize.Phase.Eval where

import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Optimize.LambdaFolding
import Juvix.Compiler.Core.Transformation.Optimize.LetFolding

optimize :: InfoTable -> InfoTable
optimize = letFolding . lambdaFolding
