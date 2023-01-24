module Juvix.Compiler.Asm.Transformation.Validate where

import Juvix.Compiler.Asm.Transformation.Base

validateCode :: forall r. (Member (Error AsmError) r) => InfoTable -> FunctionInfo -> Code -> Sem r Code
validateCode tab fi code = do
  recurse sig (argumentsFromFunctionInfo fi) code
  return code
  where
    sig :: RecursorSig Memory r ()
    sig =
      RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = \_ _ -> return (),
          _recurseBranch = \_ _ _ _ -> return (),
          _recurseCase = \_ _ _ _ -> return ()
        }

validateFunction :: (Member (Error AsmError) r) => InfoTable -> FunctionInfo -> Sem r FunctionInfo
validateFunction tab fi = liftCodeTransformation (validateCode tab fi) fi

validate :: (Member (Error AsmError) r) => InfoTable -> Sem r InfoTable
validate tab = liftFunctionTransformation (validateFunction tab) tab

validate' :: InfoTable -> Maybe AsmError
validate' tab =
  case run $ runError $ validate tab of
    Left err -> Just err
    _ -> Nothing
