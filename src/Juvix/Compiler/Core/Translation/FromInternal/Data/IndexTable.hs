module Juvix.Compiler.Core.Translation.FromInternal.Data.IndexTable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Core.Language

data IndexTable = IndexTable
  { _indexTableVarsNum :: Index,
    _indexTableVars :: HashMap NameId Index
  }

makeLenses ''IndexTable

initIndexTable :: IndexTable
initIndexTable = IndexTable 0 mempty

localAddName :: forall r a. (Member (Reader IndexTable) r) => Name -> Sem r a -> Sem r a
localAddName n s = do
  updateFn <- update
  local updateFn s
  where
    update :: Sem r (IndexTable -> IndexTable)
    update = do
      idx <- asks (^. indexTableVarsNum)
      return
        ( over indexTableVars (HashMap.insert (n ^. nameId) idx)
            . over indexTableVarsNum (+ 1)
        )
