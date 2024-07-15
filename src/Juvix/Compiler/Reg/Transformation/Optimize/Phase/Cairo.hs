module Juvix.Compiler.Reg.Transformation.Optimize.Phase.Cairo where

import Juvix.Compiler.Reg.Transformation.Base
import Juvix.Compiler.Reg.Transformation.Optimize.BranchToIf
import Juvix.Compiler.Reg.Transformation.Optimize.DeadCodeElimination
import Juvix.Compiler.Reg.Transformation.Optimize.Phase.Main qualified as Main

optimize :: (Member (Reader Options) r) => InfoTable -> Sem r InfoTable
optimize =
  withOptimizationLevel 1
    $ Main.optimize
    >=> return
    . removeDeadAssignments
    . convertBranchOnZeroToIf
