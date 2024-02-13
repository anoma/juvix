module Juvix.Compiler.Asm.Transformation.StackUsage where

import Juvix.Compiler.Asm.Transformation.Base

computeFunctionStackUsage :: (Member (Error AsmError) r) => InfoTable -> FunctionInfo -> Sem r FunctionInfo
computeFunctionStackUsage tab fi = do
  ps <- recurseS sig (fi ^. functionCode)
  let maxValueStack = maximum (map fst ps)
      maxTempStack = maximum (map snd ps)
      extra =
        FunctionInfoExtra
          { _functionMaxValueStackHeight = maxValueStack,
            _functionMaxTempStackHeight = maxTempStack
          }
  return
    fi
      { _functionExtra = Just extra
      }
  where
    sig :: RecursorSig StackInfo r (Int, Int)
    sig =
      RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = \si _ -> return (si ^. stackInfoValueStackHeight, si ^. stackInfoTempStackHeight),
          _recurseBranch = \_ si _ l r ->
            return
              ( max (si ^. stackInfoValueStackHeight) (max (maximum (map fst l)) (maximum (map fst r))),
                max (si ^. stackInfoTempStackHeight) (max (maximum (map snd l)) (maximum (map snd r)))
              ),
          _recurseCase = \_ si _ cs md ->
            return
              ( max (si ^. stackInfoValueStackHeight) (max (maximum (map (maximum . map fst) cs)) (maybe 0 (maximum . map fst) md)),
                max (si ^. stackInfoTempStackHeight) (max (maximum (map (maximum . map snd) cs)) (maybe 0 (maximum . map snd) md))
              ),
          _recurseSave = \si _ b ->
            return
              ( max (si ^. stackInfoValueStackHeight) (maximum (map fst b)),
                max (si ^. stackInfoTempStackHeight) (maximum (map snd b))
              )
        }

computeStackUsage :: (Member (Error AsmError) r) => InfoTable -> Sem r InfoTable
computeStackUsage tab = liftFunctionTransformation (computeFunctionStackUsage tab) tab
