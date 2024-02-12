module Juvix.Compiler.Reg.Transformation.Identity where

import Juvix.Compiler.Reg.Extra.Recursors
import Juvix.Compiler.Reg.Transformation.Base

identity :: InfoTable -> InfoTable
identity = mapT (const (cmap id))
