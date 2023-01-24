module Juvix.Compiler.Asm.Transformation.Base
  ( module Juvix.Compiler.Asm.Transformation.Base,
    module Juvix.Compiler.Asm.Data.InfoTable,
    module Juvix.Compiler.Asm.Extra,
    module Juvix.Compiler.Asm.Language,
  )
where

import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Language

liftCodeTransformation :: (Monad m) => (Code -> m Code) -> FunctionInfo -> m FunctionInfo
liftCodeTransformation f fi = do
  code <- f (fi ^. functionCode)
  return fi {_functionCode = code}

liftFunctionTransformation :: (Monad m) => (FunctionInfo -> m FunctionInfo) -> InfoTable -> m InfoTable
liftFunctionTransformation f tab = do
  fns <- mapM f (tab ^. infoFunctions)
  return tab {_infoFunctions = fns}

runTransformation :: (InfoTable -> Sem '[Error AsmError] InfoTable) -> InfoTable -> Either AsmError InfoTable
runTransformation trans tab =
  case run $ runError $ trans tab of
    Left err -> Left err
    Right tab' -> Right tab'
