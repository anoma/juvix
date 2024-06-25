module Juvix.Compiler.Reg.Transformation.Cleanup where

import Juvix.Compiler.Reg.Extra.Base (updateLiveVars')
import Juvix.Compiler.Reg.Extra.Recursors
import Juvix.Compiler.Reg.Transformation.Base
import Juvix.Compiler.Tree.Extra.Rep

cleanup' :: Bool -> InfoTable -> InfoTable
cleanup' bCairo tab = mapT (const (cmap go)) tab
  where
    go :: Code -> Code
    go = \case
      Nop : is -> is
      Block InstrBlock {..} : is -> _instrBlockCode ++ is
      Case InstrCase {..} : is
        | isInductiveRecord tab _instrCaseInductive -> case _instrCaseBranches of
            CaseBranch {..} : _ ->
              _caseBranchCode ++ is
            [] ->
              fromJust _instrCaseDefault ++ is
      Prealloc {} : is | bCairo -> is
      i : is | bCairo -> updateLiveVars' (const Nothing) i : is
      is -> is

cleanup :: InfoTable -> InfoTable
cleanup = cleanup' False
