module Juvix.Compiler.Asm.Transformation.TempHeight where

import Juvix.Compiler.Asm.Data.Stack
import Juvix.Compiler.Asm.Transformation.Base

computeFunctionTempHeight ::
  (Member (Error AsmError) r) =>
  InfoTable ->
  FunctionInfo ->
  Sem r FunctionInfo
computeFunctionTempHeight tab fi = do
  ps :: [Command] <- recurseFun sig fi
  return (set functionCode ps fi)
  where
    sig :: RecursorSig Memory r Command
    sig =
      RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = goInstr,
          _recurseBranch = goBranch,
          _recurseCase = goCase,
          _recurseSave = goSave
        }

    goInstr :: Memory -> CmdInstr -> Sem r Command
    goInstr mem cmd@(CmdInstr _ instr) = case instr of
      Push (Ref (DRef (TempRef r))) ->
        let h = mem ^. memoryTempStack . stackHeight
            r' = set refTempTempHeight (Just h) r
            instr' = Push (Ref (DRef (TempRef r')))
         in return (Instr (set cmdInstrInstruction instr' cmd))
      Push (Ref (ConstrRef field@Field {_fieldRef = TempRef r})) ->
        let h = mem ^. memoryTempStack . stackHeight
            r' = set refTempTempHeight (Just h) r
            instr' =
              Push
                ( Ref
                    ( ConstrRef
                        field
                          { _fieldRef = TempRef r'
                          }
                    )
                )
         in return (Instr (set cmdInstrInstruction instr' cmd))
      _ -> return (Instr cmd)

    goCase :: Memory -> CmdCase -> [Code] -> Maybe Code -> Sem r Command
    goCase _ cmd brs mdef =
      return
        ( Case
            cmd
              { _cmdCaseBranches = branches',
                _cmdCaseDefault = mdef
              }
        )
      where
        branches' :: [CaseBranch]
        branches' =
          [ set caseBranchCode newCode oldBr
            | (oldBr, newCode) <- zipExact (cmd ^. cmdCaseBranches) brs
          ]

    goBranch :: Memory -> CmdBranch -> Code -> Code -> Sem r Command
    goBranch _ cmd t f =
      return
        ( Branch
            cmd
              { _cmdBranchTrue = t,
                _cmdBranchFalse = f
              }
        )
    goSave :: Memory -> CmdSave -> Code -> Sem r Command
    goSave _ cmd code = return (Save (set cmdSaveCode code cmd))

computeTempHeight :: (Member (Error AsmError) r) => InfoTable -> Sem r InfoTable
computeTempHeight tab = liftFunctionTransformation (computeFunctionTempHeight tab) tab
