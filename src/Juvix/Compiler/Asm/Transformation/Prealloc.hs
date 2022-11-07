module Juvix.Compiler.Asm.Transformation.Prealloc where

import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Transformation.Base

computeCodePrealloc :: forall r. Members '[Error AsmError, Reader Options] r => InfoTable -> Code -> Sem r Code
computeCodePrealloc tab code = prealloc <$> foldS sig code (0, [])
  where
    -- returns the maximum memory use and the mapping result (Code with the
    -- Prealloc instructions inserted)
    sig :: FoldSig StackInfo r (Int, Code)
    sig =
      FoldSig
        { _foldInfoTable = tab,
          _foldAdjust = second (const []),
          _foldInstr = const goInstr,
          _foldBranch = const goBranch,
          _foldCase = const goCase
        }

    goInstr :: CmdInstr -> (Int, Code) -> Sem r (Int, Code)
    goInstr instr@CmdInstr {..} acc@(k, c) = case _cmdInstrInstruction of
      AllocConstr tag ->
        return (k + size, cmd : c)
        where
          ci = getConstrInfo tab tag
          size = getConstrSize (ci ^. constructorRepresentation) (ci ^. constructorArgsNum)
      AllocClosure InstrAllocClosure {..} -> do
        opts <- ask
        let size = getClosureSize opts _allocClosureArgsNum
        return (k + size, cmd : c)
      ExtendClosure {} -> do
        opts <- ask
        let size = opts ^. optLimits . limitsMaxClosureSize
        return (k + size, cmd : c)
      Call {} -> return (0, cmd : prealloc acc)
      TailCall {} -> return (0, cmd : prealloc acc)
      CallClosures {} -> return (0, cmd : prealloc acc)
      TailCallClosures {} -> return (0, cmd : prealloc acc)
      _ -> return (k, cmd : c)
      where
        cmd = Instr instr

    goBranch :: CmdBranch -> (Int, Code) -> (Int, Code) -> (Int, Code) -> Sem r (Int, Code)
    goBranch cmd br1 br2 acc = return (0, cmd' : prealloc acc)
      where
        cmd' =
          Branch
            cmd
              { _cmdBranchTrue = prealloc br1,
                _cmdBranchFalse = prealloc br2
              }

    goCase :: CmdCase -> [(Int, Code)] -> Maybe (Int, Code) -> (Int, Code) -> Sem r (Int, Code)
    goCase cmd brs md acc = return (0, cmd' : prealloc acc)
      where
        cmd' =
          Case
            cmd
              { _cmdCaseBranches =
                  zipWith
                    CaseBranch
                    (map (^. caseBranchTag) (cmd ^. cmdCaseBranches))
                    (map prealloc brs),
                _cmdCaseDefault = fmap prealloc md
              }

    prealloc :: (Int, Code) -> Code
    prealloc (0, c) = c
    prealloc (n, c) = mkInstr (Prealloc (InstrPrealloc n)) : c

computeFunctionPrealloc :: Members '[Error AsmError, Reader Options] r => InfoTable -> FunctionInfo -> Sem r FunctionInfo
computeFunctionPrealloc tab = liftCodeTransformation (computeCodePrealloc tab)

computePrealloc :: Members '[Error AsmError, Reader Options] r => InfoTable -> Sem r InfoTable
computePrealloc tab = liftFunctionTransformation (computeFunctionPrealloc tab) tab
