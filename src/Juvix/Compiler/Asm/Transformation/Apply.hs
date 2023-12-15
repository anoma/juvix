module Juvix.Compiler.Asm.Transformation.Apply where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Extra.Apply
import Juvix.Compiler.Asm.Options
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
      CallClosures InstrCallClosures {..} -> return $ goApply False _callClosuresArgsNum
      TailCallClosures InstrCallClosures {..} -> return $ goApply True _callClosuresArgsNum
      _ -> return [Instr cmd]

    goApply :: Bool -> Int -> Code
    goApply isTail n = replicate m (mkApply False (blts ^. applyBuiltinsNum)) ++ [mkApply isTail r]
      where
        (m, r) = n `divMod` (blts ^. applyBuiltinsNum)

    mkApply :: Bool -> Int -> Command
    mkApply isTail k =
      Instr $
        CmdInstr emptyInfo $
          (if isTail then TailCall else Call)
            InstrCall
              { _callType = CallFun sym,
                _callArgsNum = k + 1
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

checkNoCallClosures :: Options -> InfoTable -> Bool
checkNoCallClosures opts tab =
  case run $ runError $ runReader opts sb of
    Left err -> error (show err)
    Right b -> b
  where
    sb :: Sem '[Reader Options, Error AsmError] Bool
    sb = allM (check . (^. functionCode)) (HashMap.elems (tab ^. infoFunctions))

    check :: Code -> Sem '[Reader Options, Error AsmError] Bool
    check c = foldS sig c True
      where
        sig =
          FoldSig
            { _foldInfoTable = tab,
              _foldAdjust = id,
              _foldInstr = \_ cmd b -> return $ b && goInstr (cmd ^. cmdInstrInstruction),
              _foldBranch = \_ _ b1 b2 b3 -> return $ b1 && b2 && b3,
              _foldCase = \_ _ bs bd b -> return $ and bs && fromMaybe True bd && b,
              _foldSave = \_ _ b1 b2 -> return $ b1 && b2
            }

        goInstr :: Instruction -> Bool
        goInstr = \case
          CallClosures {} -> False
          TailCallClosures {} -> False
          _ -> True
