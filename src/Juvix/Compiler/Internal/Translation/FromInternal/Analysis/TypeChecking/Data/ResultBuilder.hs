module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.ResultBuilder where

import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Prelude

data ResultBuilder :: Effect where
  AddFunctionDef :: Name -> Expression -> ResultBuilder m ()
  AddIdenType :: NameId -> Expression -> ResultBuilder m ()
  LookupFunctionDef :: Name -> ResultBuilder m (Maybe Expression)
  LookupIdenType :: NameId -> ResultBuilder m (Maybe Expression)

makeSem ''ResultBuilder
