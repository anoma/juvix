module Juvix.Compiler.Reg.Extra where

import Juvix.Compiler.Backend
import Juvix.Compiler.Reg.Language

-- | Compute the maximum runtime stack height
computeMaxStackHeight :: Limits -> Code -> Int
computeMaxStackHeight lims = maximum . map go
  where
    go :: Instruction -> Int
    go = \case
      Nop -> 0
      Binop {} -> 0
      Assign {} -> 0
      Trace {} -> 0
      Dump -> 0
      Failure {} -> 0
      Prealloc InstrPrealloc {..} ->
        length _instrPreallocLiveVars
      Alloc {} -> 0
      AllocClosure {} -> 0
      ExtendClosure {} -> 0
      Call InstrCall {..} ->
        length _instrCallLiveVars + 1
      CallClosures InstrCallClosures {..} ->
        length _instrCallClosuresLiveVars
          + length _instrCallClosuresArgs
          + lims
          ^. limitsDispatchStackSize
      Return -> 0
      Branch InstrBranch {..} ->
        max
          (computeMaxStackHeight lims _instrBranchTrue)
          (computeMaxStackHeight lims _instrBranchFalse)
      Case InstrCase {..} ->
        max
          ( maximum
              ( map
                  (computeMaxStackHeight lims . (^. caseBranchCode))
                  _instrCaseBranches
              )
          )
          (maybe 0 (computeMaxStackHeight lims) _instrCaseDefault)
