module Juvix.Compiler.Asm.Transformation.TempHeight where

import Juvix.Compiler.Asm.Transformation.Base

computeFunctionTempHeight ::
  (Member (Error AsmError) r) =>
  InfoTable ->
  FunctionInfo ->
  Sem r FunctionInfo
computeFunctionTempHeight tab fi = do
  ps :: [Command] <- recurseS sig (fi ^. functionCode)
  return (set functionCode ps fi)
  where
    sig :: RecursorSig StackInfo r Command
    sig =
      RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = goInstr,
          _recurseBranch = goBranch,
          _recurseCase = goCase,
          _recurseSave = goSave
        }

    goInstr :: StackInfo -> CmdInstr -> Sem r Command
    goInstr si cmd@(CmdInstr _ instr) = case instr of
      Push (Ref (DRef (TempRef r))) ->
        let h = si ^. stackInfoTempStackHeight
            r' = set refTempTempHeight (Just h) r
            instr' = Push (Ref (DRef (TempRef r')))
         in return (Instr (set cmdInstrInstruction instr' cmd))
      Push (Ref (ConstrRef field@Field {_fieldRef = TempRef r})) ->
        let h = si ^. stackInfoTempStackHeight
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

    goCase :: StackInfo -> CmdCase -> [Code] -> Maybe Code -> Sem r Command
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

    goBranch :: StackInfo -> CmdBranch -> Code -> Code -> Sem r Command
    goBranch _ cmd t f =
      return
        ( Branch
            cmd
              { _cmdBranchTrue = t,
                _cmdBranchFalse = f
              }
        )
    goSave :: StackInfo -> CmdSave -> Code -> Sem r Command
    goSave _ cmd code = return (Save (set cmdSaveCode code cmd))

computeTempHeight :: (Member (Error AsmError) r) => InfoTable -> Sem r InfoTable
computeTempHeight tab = liftFunctionTransformation (computeFunctionTempHeight tab) tab
