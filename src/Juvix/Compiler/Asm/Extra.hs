module Juvix.Compiler.Asm.Extra
  ( module Juvix.Compiler.Asm.Extra,
    module Juvix.Compiler.Asm.Extra.Base,
    module Juvix.Compiler.Asm.Extra.Type,
    module Juvix.Compiler.Asm.Extra.Recursors,
    module Juvix.Compiler.Asm.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Recursors
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language

validateCode :: forall r. Member (Error AsmError) r => InfoTable -> Arguments -> Code -> Sem r ()
validateCode tab args = void . recurse sig args
  where
    sig :: RecursorSig Memory r ()
    sig =
      RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = \_ _ -> return (),
          _recurseBranch = \_ _ _ _ -> return (),
          _recurseCase = \_ _ _ _ -> return ()
        }

validateFunction :: Member (Error AsmError) r => InfoTable -> FunctionInfo -> Sem r ()
validateFunction tab fi = validateCode tab (argumentsFromFunctionInfo fi) (fi ^. functionCode)

validateInfoTable :: Member (Error AsmError) r => InfoTable -> Sem r InfoTable
validateInfoTable tab = do
  mapM_ (validateFunction tab) (HashMap.elems (tab ^. infoFunctions))
  return tab

validate :: InfoTable -> Maybe AsmError
validate tab =
  case run $ runError $ validateInfoTable tab of
    Left err -> Just err
    _ -> Nothing

computeFunctionStackUsage :: Member (Error AsmError) r => InfoTable -> FunctionInfo -> Sem r FunctionInfo
computeFunctionStackUsage tab fi = do
  ps <- snd <$> recurseS sig initialStackInfo (fi ^. functionCode)
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

computeStackUsage :: Member (Error AsmError) r => InfoTable -> Sem r InfoTable
computeStackUsage tab = do
  fns <- mapM (computeFunctionStackUsage tab) (tab ^. infoFunctions)
  return tab {_infoFunctions = fns}

computeStackUsage' :: InfoTable -> Either AsmError InfoTable
computeStackUsage' tab = run $ runError $ computeStackUsage tab
