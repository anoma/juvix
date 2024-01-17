module Juvix.Compiler.Asm.Transformation.FilterUnreachable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.CallGraph
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error

filterUnreachable :: (Member (Error AsmError) r) => InfoTable -> Sem r InfoTable
filterUnreachable tab = do
  graph <- createCallGraph tab
  return $ over infoFunctions (HashMap.filterWithKey (const . isReachable graph)) tab
