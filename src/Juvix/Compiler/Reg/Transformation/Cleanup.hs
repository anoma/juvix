module Juvix.Compiler.Reg.Transformation.Cleanup where

import Juvix.Compiler.Reg.Extra.Recursors
import Juvix.Compiler.Reg.Transformation.Base

cleanup :: InfoTable -> InfoTable
cleanup = mapT (const (cmap go))
  where
    go :: Code -> Code
    go = \case
      Nop : is -> is
      Block InstrBlock {..} : is -> _instrBlockCode ++ is
      is -> is
