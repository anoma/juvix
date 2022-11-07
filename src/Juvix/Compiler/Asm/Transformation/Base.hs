module Juvix.Compiler.Asm.Transformation.Base
  ( module Juvix.Compiler.Asm.Transformation.Base,
    module Juvix.Compiler.Asm.Data.InfoTable,
    module Juvix.Compiler.Asm.Extra,
    module Juvix.Compiler.Asm.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra
import Juvix.Compiler.Asm.Language

liftCodeTransformation :: Monad m => (Code -> m Code) -> FunctionInfo -> m FunctionInfo
liftCodeTransformation f fi = do
  code <- f (fi ^. functionCode)
  return fi {_functionCode = code}

liftFunctionTransformation :: Monad m => (FunctionInfo -> m FunctionInfo) -> InfoTable -> m InfoTable
liftFunctionTransformation f tab = do
  lst <- mapM (secondM f) (HashMap.toList (tab ^. infoFunctions))
  return tab {_infoFunctions = HashMap.fromList lst}
