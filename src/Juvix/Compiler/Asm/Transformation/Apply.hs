module Juvix.Compiler.Asm.Transformation.Apply where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Extra.Apply
import Juvix.Compiler.Asm.Transformation.Base

computeFunctionApply :: (Member (Error AsmError) r) => ApplyBuiltins -> InfoTable -> FunctionInfo -> Sem r FunctionInfo
computeFunctionApply blts tab fi = do
  cs <- recurseS sig (fi ^. functionCode)
  return fi {_functionCode = concat cs}
  where
    sig :: RecursorSig StackInfo r Code
    sig =
      RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = \_ cmd -> goInstr cmd,
          _recurseBranch = \_ cmd l r -> goBranch cmd l r,
          _recurseCase = \_ cmd cs md -> goCase cmd cs md,
          _recurseSave = \_ cmd b -> goSave cmd b
        }

    goInstr :: CmdInstr -> Sem r Code
    goInstr cmd = case cmd ^. cmdInstrInstruction of
      CallClosures InstrCallClosures {..} -> return $ goApply _callClosuresArgsNum
      TailCallClosures InstrCallClosures {..} -> return $ goApply _callClosuresArgsNum
      _ -> return [Instr cmd]

    goApply :: Int -> Code
    goApply n = replicate m (mkApply (blts ^. applyBuiltinsNum)) ++ [mkApply r]
      where
        (m, r) = n `divMod` (blts ^. applyBuiltinsNum)

    mkApply :: Int -> Command
    mkApply k =
      Instr $
        CmdInstr emptyInfo $
          Call
            InstrCall
              { _callType = CallFun sym,
                _callArgsNum = k
              }
      where
        sym = fromJust $ HashMap.lookup k (blts ^. applyBuiltinsMap)

    goBranch :: CmdBranch -> [Code] -> [Code] -> Sem r Code
    goBranch cmd l r =
      return
        [ Branch
            cmd
              { _cmdBranchTrue = concat l,
                _cmdBranchFalse = concat r
              }
        ]

    goCase :: CmdCase -> [[Code]] -> Maybe [Code] -> Sem r Code
    goCase cmd cs md =
      return
        [ Case
            cmd
              { _cmdCaseBranches =
                  zipWith
                    (\br c -> CaseBranch (br ^. caseBranchTag) (concat c))
                    (cmd ^. cmdCaseBranches)
                    cs,
                _cmdCaseDefault = fmap concat md
              }
        ]

    goSave :: CmdSave -> [Code] -> Sem r Code
    goSave cmd c = return [Save cmd {_cmdSaveCode = concat c}]

computeApply :: (Member (Error AsmError) r) => InfoTable -> Sem r InfoTable
computeApply tab = liftFunctionTransformation (computeFunctionApply blts tab') tab'
  where
    (blts, tab') = addApplyBuiltins tab
