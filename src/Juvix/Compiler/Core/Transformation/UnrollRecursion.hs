module Juvix.Compiler.Core.Transformation.UnrollRecursion(unrollRecursion) where

import Juvix.Compiler.Core.Transformation.Base

-- TODO: not yet implemented / at first only check for recursion and give an error
unrollRecursion :: InfoTable -> InfoTable
unrollRecursion tab = tab
