module Juvix.Compiler.Asm.Transformation.StackUsage where

import Juvix.Compiler.Asm.Transformation.Base

computeFunctionStackUsage :: (Member (Error AsmError) r) => InfoTable -> FunctionInfo -> Sem r FunctionInfo
computeFunctionStackUsage tab fi = do
  ps <- recurseS sig (fi ^. functionCode)
  let maxValueStack = maximum (map fst ps)
      maxTempStack = maximum (map snd ps)
  return
    fi
      { _functionMaxValueStackHeight = maxValueStack,
        _functionMaxTempStackHeight = maxTempStack
      }
  where
    sig :: RecursorSig StackInfo r (Int, Int)
    sig =
      RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = \si _ -> return (si ^. stackInfoValueStackHeight, si ^. stackInfoTempStackHeight),
          _recurseBranch = \si _ l r ->
            return
              ( max (si ^. stackInfoValueStackHeight) (max (maximum (map fst l)) (maximum (map fst r))),
                max (si ^. stackInfoTempStackHeight) (max (maximum (map snd l)) (maximum (map snd r)))
              ),
          _recurseCase = \si _ cs md ->
            return
              ( max (si ^. stackInfoValueStackHeight) (max (maximum (map (maximum . map fst) cs)) (maybe 0 (maximum . map fst) md)),
                max (si ^. stackInfoTempStackHeight) (max (maximum (map (maximum . map snd) cs)) (maybe 0 (maximum . map snd) md))
              )
        }

computeStackUsage :: (Member (Error AsmError) r) => InfoTable -> Sem r InfoTable
computeStackUsage tab = liftFunctionTransformation (computeFunctionStackUsage tab) tab
