module Juvix.Compiler.Core.Translation.FromInternal.Data.IndexTable where

import Juvix.Compiler.Core.Language

data IndexTable = IndexTable
  { _indexTableVarsNum :: Index,
    _indexTableVars :: HashMap NameId Index
  }

makeLenses ''IndexTable

initIndexTable :: IndexTable
initIndexTable = IndexTable 0 mempty
